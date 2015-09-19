load("MOOC_image.RData")

function(input,output){

  #output file for the download button 
  output$sample_data<-downloadHandler(
    filename = function() {
      paste('sample_data_set.csv')
    },
    content = function(file) {
      write.csv(data_test_new,file)
    }
  )
  
  #output file for first tab
  output$raw_output<-renderPrint({
    print(rf_model2)
    print(summary(rf_model2))
  })
  
  #input file for prediction data
  predict_data_read<-reactive({
    data<-input$predict
    if(is.null(data)) return(NULL)
    data<-read.csv(data$datapath)
  })
  
  #run the prediction function
  predict_data<-reactive({
    data_test <- predict_data_read()
    empty_data <- as.data.frame("PLEASE UPLOAD A DATASET")
    names(empty_data)<-NULL
    if(is.null(data_test)){
      return(empty_data)
    }
    table1 <- as.data.frame(predict(rf_model2,data_test,type='prob'))
    table2 <- as.data.frame(predict(rf_model2,data_test))
    names(table2)[1] <- "Prediction_Class"
    output <- cbind(table1,table2)
    return(output)
  })
  
  #output for second tab
  output$predict_table<-renderTable({
    data <- predict_data()
    return(data)
  })
  
  output$prediction_table<-downloadHandler(
    
    filename = function() {
      paste('prediction.csv')
    },
    content = function(file) {
      write.csv(predict_data(),file,row.names=FALSE)
    }
  )
}