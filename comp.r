# compares two data frames for t test

# size3_offer <- bymeans( "offer" , "emp_a6x.d" , "size3" , TRUE , testing="groups" , suppress = FALSE )
# size3_offer_m01 <- bymeans( "offer" , "emp_a6x.d_m01" , "size3" , TRUE , testing="groups" , suppress = FALSE )
# df1 = size3_offer
# df2 = size3_offer_m01


comparison <- 
    function ( df1 , df2 , cols = NULL , reduce=FALSE){
        
        
        
        # Example: comparison( size3_all_spouses , size3_all_spouses_m01 , cols = c( "factor_1" , "se.factor_1" ) )
        # DEFAULTS to cols=("mean","mean.1")
        # COLS - must be an estimate and a standard error in whith the same name in each file: c( "factor_1" , "se.factor_1" ) )
        
        df1 <- data.frame( df1 )
        df2 <- data.frame( df2 )
        
        if( 
            is.null(cols) & 
                !any(grepl( "mean", colnames( df1 ))) & 
                any(grepl( "factor_1", colnames( df1 ) )) &
                any(grepl( "factor_1", colnames( df2 ) ))
        ){
            
            warning("you are stat testing factor_1 - if you dont want that use the cols parameter")
            cols <- c("factor_1","se.factor_1")
        }
        
        if( is.null( cols ) ){
            
            first.col.df1 <- which( names( df1 ) == 'mean' )
            first.col.df2 <- which( names( df2 ) == 'mean' )
            
            names( df1 )[ first.col.df1:(first.col.df1 + 1 ) ] <- c( 'mean' , 'SE' )
            names( df2 )[ first.col.df2:(first.col.df2 + 1 ) ] <- c( 'mean.1' , 'SE.1' )
            
        } else {
            
            if( length( cols ) != 2 ) stop( "cols= length must be two" )
            
            names( df1 )[ names( df1 ) == cols[1] ] <- 'mean'
            names( df1 )[ names( df1 ) == cols[2] ] <- 'SE'
            names( df2 )[ names( df2 ) == cols[1] ] <- 'mean.1'
            names( df2 )[ names( df2 ) == cols[2] ] <- 'SE.1'
            
        }
        
        ### CALC STAT TEST
        a <- cbind( df1 , df2 )
        
        a <- transform ( a , t_statistic = abs( mean - mean.1 ) / sqrt (SE^2 + SE.1^2))
        a <- transform( a , p_value = ( 1 - pnorm( abs( mean - mean.1 ) / sqrt( SE^2 + SE.1^2 ) ) ) * 2 )
        a <- transform( a , sig_diff = ifelse( 1 - pnorm( abs( mean - mean.1 ) / sqrt( SE^2 + SE.1^2 ) ) < 0.025 , "*" , "" ) )
        
        
        if( any( names( a) %in% "counts" ))	if( min( a$counts ) < 30 ) print( "CHECK NUMBER OF RESPONDENTS")
        
        if( any( names( a) %in% "counts.1" ))	if( min( a[ , "counts.1"] ) < 30 ) print( "CHECK NUMBER OF RESPONDENTS - DATAFRAME 2")		
        
        
        ### DROPPING COLS AND ROUNDING
        if( reduce ){
            
            a <- a[ , c("mean","mean.1","sig_diff") ]
            if( max( a$mean < 1)){
                a[ , 1:2 ] <-  round( a[ , 1:2 ] , 2 )
            }else{
                a[ , 1:2 ] <-  round( a[ , 1:2 ]  )		
            }
            
        }
        
        a
    }

