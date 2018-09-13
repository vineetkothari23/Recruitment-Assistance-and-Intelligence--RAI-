library(shinydashboard)
library(neuralnet)
library(e1071)
#library(glm2)
library(audio)
library(shiny)
library(neuralnet)
library(shinyRGL)
library(PraatR)
library(fmsb)
library(xlsx)
library(randomForest)
library(tm)
library(quanteda)
library(qdap)
library(FSelector)
library(readtext)

options(shiny.maxRequestSize=60*1024^2) 
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"


sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Input", icon = icon("th"), tabName = "Input"),
    menuItem("Oration Analysis", icon = icon("microphone",lib="font-awesome"), tabName = "Analysis"),
    menuItem("Resume Analysis", icon = icon("file"), tabName = "ResumeAnalysis"),
    menuItem("Best Resumes", icon = icon("line-chart"), tabName = "OverallResumeAnalysis")
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("Input",
            
           
            fluidRow(
                      
                      box(
                        title = "Interviewee details", width = 4, solidHeader = TRUE, status = "primary",
                        textInput("Date","Date", value = Sys.Date()),
                        textInput("Name", "Name"),
                        textInput("Post Applied for", "Post Applied for"),
                        radioButtons("gender", "Select gender", c(Male = "Male", Female = "Female", Other="Other")),
                        #actionButton(inputId = "save", label = "Save"),
                        #actionButton(inputId = "click", label = "Next Recording")
                        fileInput("resume","Upload a resume")
                      ),#end of box
                       box(
                        title = "Record or Upload here", width = 4, solidHeader = TRUE,status="success",
                        tags$head(HTML('
                                    <head> 
                                       <script src="https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.6.0/p5.js"></script>
                                       <script src="https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.6.0/addons/p5.sound.js"></script>
                                       </head>
                                       ')),
                        numericInput("Rec_no","Question_no",value = 1),
                        radioButtons("r_u","Select mode of input", c(Record = "Record", Upload = "Upload")),
                        tags$div(tags$button(id="Record","Record"),
                                 tags$button(id="Stop","Stop"),
                                 tags$button(id="Save","Save")),
                        
                        tags$script('
                                    var mic, recorder, soundFile;
                                    
                                    var state = 0; // mousePress will increment from Record, to Stop, to Play
                                    
                                    function setup() {
                                    
                                    // create an audio in
                                    mic = new p5.AudioIn();
                                    
                                    // users must manually enable their browser microphone for recording to work properly!
                                    mic.start();
                                    
                                    // create a sound recorder
                                    recorder = new p5.SoundRecorder();
                                    
                                    // connect the mic to the recorder
                                    recorder.setInput(mic);
                                    
                                    // create an empty sound file that we will use to playback the recording
                                    soundFile = new p5.SoundFile();
                                    }
                                    document.getElementById("Record").onclick = function() {
                                    if (mic.enabled) {
                                    recorder.record(soundFile);
                                    }
                                    };
                                    document.getElementById("Stop").onclick = function() {
                                    recorder.stop();
                                    };
                                    document.getElementById("Save").onclick = function() {
                                    temp=0;
                                    soundFile.play();
                                    saveSound(soundFile, "rec.wav");
                                    Shiny.onInputChange("s1",soundFile);
                                    temp=1;
                                    Shiny.onInputChange("temp",temp);
                                    
                                    };'),#end of script
              #h4(helpText("Upload a wav file....")),
              fileInput("wavaudio","Upload a wav file....",accept = ".wav"),
              
              submitButton("Run")
              #actionButton(inputId = "new_r", label = "New Recording")
              ))),#end of box
    tabItem("Analysis",
            fluidRow(       
              
              valueBoxOutput("confidence"),
              valueBoxOutput("fluency"),
              valueBoxOutput("energetic"),
              valueBoxOutput("clarity")),
            fluidRow(
              
              box(
                title = "Extracted_values",
                status = "primary",
                tableOutput("temp"),
                height = 200,
                width=500
              )#end of box
            ),#end of fluid row
            fluidRow(
              box(
                title = "Analysis of all questions",
                
                width = 6,
                tabsetPanel(       
                  tabPanel("Confidence", plotOutput("all_questions_conf")),
                  tabPanel("Fluency", plotOutput("all_questions_flu")),
                  tabPanel("Energetic", plotOutput("all_questions_ener")),
                  tabPanel("Clarity", plotOutput("all_questions_clar"))
                  
                ),#end of tabsetpanel
                height = 500
              
                
              ), #end of box
              box(
                title = "Random Forest",
                tableOutput("randomforest_results"),
                height = 500
              )#end of box
              
            ),#end of fluid row
            fluidRow(
              
                    box(
                      title = "ANN",
                      status = "primary",
                      tabsetPanel(       
                        tabPanel("ANN", tableOutput("ann")),
                        tabPanel("Barplot", plotOutput("ann_plot"))
                        ),#end of tabset panel
                      
                      height = 500
                    ),#end of box
                    
                    box(
                      title = "SVM",
                      status = "primary",
                      tabsetPanel(       
                        tabPanel("SVM", tableOutput("svm")),
                        tabPanel("Radar Chart", plotOutput("svm_plot"))
                      ), #end of tabset panel
    
                      height = 500
                    )#end of box
            )#end of fluid row
                    
            ), #end of tabset item
    tabItem("ResumeAnalysis",
            
          
              fluidRow(
                box(title="Development",plotOutput("resume_developer"),width = 4,height=350),
                box(title="Administration",plotOutput("resume_admin"),width = 4,height=350),
                box(title="Analysis",plotOutput("resume_analyst"),width = 4,height=350)
              
              ),#end of fluid row
              fluidRow(
                valueBoxOutput("resume_complexity"),
                valueBoxOutput("resume_uniqueness"),
                textOutput("resume_sentences")
              )#end of fluid row
            
            
      
    ),#end of tabset item
    tabItem("dashboard" ,
            fluidRow(
                box(
                title="Complete Recruitment Analysis",
                tabsetPanel(
                  tabPanel("Confidence", plotOutput("all_interviewee_conf")),
                  tabPanel("Fluency", plotOutput("all_interviewee_flu")),
                  tabPanel("Energetic", plotOutput("all_interviewee_ener")),
                  tabPanel("Clarity", plotOutput("all_interviewee_clar"))
                  
                ),#end of tabset panel
                height=500,
                width = 12
              )#end of box
              
            )#end of fluid row
            ),#end of tabset item
    tabItem("OverallResumeAnalysis" ,
            fluidRow(
              box(
                title="Job profile",
                job_profile<-c('Developer','Analyst','Administrator'),
                selectInput('jobprof', 'Select Job Profile',job_profile),
                height=400,
                width = 3
              ),
              box(
                title="Add job specialisation",
                #if(jobprof=="Analyst"){job_spec_dev<-c("a","b","c")},
                numericInput("num_resume","No. of resumes to be extracted",min=1,max=10,value = 3),
                job_spec_dev<-c('Cross platform Mobile Development','Cloud integration and migration','Web development','Database handling and management'),
                checkboxGroupInput('jobspec_dev', 'Select job specialisation for developer', job_spec_dev),
                height = 400,
                width=5
              ),#end of box
              box(
                title="Selected resumes according to specialisation",
                tableOutput("ora"),
                height = 400,
                width=4
              )#end of box
            ),# end of fluid row
              fluidRow(
                     box(
                       title="Top 10 resumes",
                       plotOutput("topTenResumes"),
                       height= 500,
                       width=12
                     )#end of box
                
              )#end of fluid row
              
            
    )#end of tabset item overall resume analysis
  )

  )



header <- dashboardHeader(
  title = "Oratory inference"
)

ui <- dashboardPage(header, sidebar, body, skin=skin)

server <- function(input, output) {
  
  
  set.seed(122)
  #histdata <- rnorm(500)
  data<-reactive({file1<-input$wavaudio
  file1<-file1$datapath
  })
  data_resume<-reactive({file_resume<-input$resume
  file_resume<-file_resume$datapath
  })
  # eventReactive(input$click, {
  #   reload
  #   FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
  #   interviewee_data<-data.frame(read.csv(file=FullPath("Data.csv"),header = T,stringsAsFactors = F))
  #   input$Name=length(interviewee_data)
  #   input$Name.value="Jell"
  # })
  
  #FullPath = function(FileName){ return( paste( "C:/BE/P1/", FileName, sep="") ) }
  extraction2<-reactive(
    {
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      temp_extract=0

      if(input$r_u=="Record"){
        while(input$temp==0) wait()
        a<-"rec.wav"
        audiopath=FullPath(a)}
      if(input$r_u=="Upload"){

        audiopath<-data()
      }
      
      energy<-as.numeric(praat("Get energy...",arguments =	list(0,0),input=audiopath,simplify = TRUE))
      power<-as.numeric(praat("Get power...",arguments =	list(0,0),input=audiopath,simplify = TRUE))
      #Create
      praat("To Pitch (ac)...",arguments=list(0,50,15,"no",0.15,0.45,0.01,0.35,0.14,300),input=audiopath,output=FullPath("4_pitch"),overwrite=TRUE)
      praat( "To Formant (burg)...",arguments = list(0.001,5,5500,0.025,50), input = audiopath,output = FullPath("4formant"),overwrite = TRUE)
      praat( "Down to Table...", arguments = list(TRUE,TRUE,3,TRUE,3,TRUE,3,TRUE),input = FullPath("4formant"),output = FullPath("4formantTable.txt"),overwrite = TRUE,filetype = "tab-separated" )
      praat( "To Intensity...",arguments = list(100, 0, "yes"), input = audiopath,output = FullPath("4intensity"),overwrite = TRUE)
      praat("To PointProcess (periodic, cc)...",arguments=list(75,600),input = audiopath,output = FullPath("4pointprocess"),overwrite = TRUE)
      praat("Down to IntensityTier",input = FullPath("4intensity"),output = FullPath("4intensitytier"),overwrite = TRUE)
      praat("To AmplitudeTier",input = FullPath("4intensitytier"),output = FullPath("4amplitudetier"),overwrite = TRUE)
      praat("Down to Matrix",input = audiopath,output = FullPath("4matrix"),overwrite = TRUE)

      #extract
      #pitch
      min_pitch <- as.numeric(praat("Get minimum...",arguments=list(0, 0, "Hertz", "Parabolic")	,input=FullPath("4_pitch"),simplify=TRUE))
      max_pitch <- as.numeric(praat("Get maximum...",arguments=list(0, 0, "Hertz", "Parabolic")	,input=FullPath("4_pitch"),simplify=TRUE))
      mean_pitch <- as.numeric(praat("Get mean...",arguments=list(0,0,"Hertz"),input=FullPath("4_pitch"),simplify=TRUE))
      pitch_sd <- as.numeric(praat("Get standard deviation...",arguments=list(0,0,"Hertz"),input=FullPath("4_pitch"),simplify=TRUE))
      #pitch_abs here
      pitch_quant <- as.numeric(praat("Get quantile...",arguments=list(0, 0, 0.5, "Hertz")	,input=FullPath("4_pitch"),simplify=TRUE))
      #pitchUvsVRatio	Time:8	iDifference here
      diffPitchMaxMin <- as.numeric(max_pitch-min_pitch)
      diffPitchMaxMean <- as.numeric(max_pitch-mean_pitch)
      #diffPitchMaxMode Here
      no_voiced_frames <- as.numeric(praat("Count voiced frames",input=FullPath("4_pitch"),simplify=TRUE))
      total_frames<- as.numeric(praat("Get number of frames",input=FullPath("4_pitch"),simplify=TRUE))
      percentUnvoiced<-(total_frames - no_voiced_frames)/total_frames
      #intensity
      intensityMin<-as.numeric(praat("Get minimum...",arguments = list(0, 0, "Parabolic")	,input = FullPath("4intensity"),simplify = TRUE))
      intensityMax<-as.numeric(praat("Get maximum...",arguments = list(0, 0, "Parabolic")	,input = FullPath("4intensity"),simplify = TRUE))
      intensityMean<-as.numeric(praat("Get mean...",arguments = list(0, 0),input = FullPath("4intensity"),simplify = TRUE))
      intensitySD<-as.numeric(praat("Get standard deviation...",arguments = list(0, 0),input = FullPath("4intensity"),simplify = TRUE))
      intensityQuant<-as.numeric(praat("Get quantile...",arguments = list(0, 0, 0.5),input = FullPath("4intensity"),simplify = TRUE))
      diffIntMaxMin <- intensityMax - intensityMin
      diffIntMaxMean <- intensityMax - intensityMean
      #diffIntMaxMode Here


      #formant
      duration<-as.numeric(praat("Get total duration",input = FullPath("4formant"),simplify = TRUE))
      duration<-round(as.numeric(duration),digits = 0)
      sum1<-0
      sum2<-0
      sum3<-0

      sumb1<-0
      sumb2<-0
      sumb3<-0

      x<-0
      dura1<-duration-1
      for(x in c(1:dura1)){
        #Val
        f1val<-as.numeric(praat("Get value at time...",arguments = list(1, x, "Hertz", "Linear")	,input = FullPath("4formant"),simplify = TRUE))
        f2val<-as.numeric(praat("Get value at time...",arguments = list(2, x, "Hertz", "Linear")	,input = FullPath("4formant"),simplify = TRUE))
        f3val<-as.numeric(praat("Get value at time...",arguments = list(3, x, "Hertz", "Linear")	,input = FullPath("4formant"),simplify = TRUE))

        #band

        f1band<-as.numeric(praat("Get bandwidth at time...",arguments = list(1, x, "Hertz", "Linear"),input = FullPath("4formant"),simplify = TRUE))
        f2band<-as.numeric(praat("Get bandwidth at time...",arguments = list(2, x, "Hertz", "Linear"),input = FullPath("4formant"),simplify = TRUE))
        f3band<-as.numeric(praat("Get bandwidth at time...",arguments = list(3, x, "Hertz", "Linear"),input = FullPath("4formant"),simplify = TRUE))

        sum1=sum1+as.numeric(f1val)
        sum2=sum2+as.numeric(f2val)
        sum3=sum3+as.numeric(f3val)
        sumb1=sumb1+as.numeric(f1band)
        sumb2=sumb2+as.numeric(f2band)
        sumb3=sumb3+as.numeric(f3band)
      }
      avgVal1<-sum1/duration
      avgVal2<-sum2/duration
      avgVal3<-sum3/duration

      avgBand1<-sumb1/duration
      avgBand2<-sumb2/duration
      avgBand3<-sumb3/duration

      fmean1<-as.numeric(praat("Get mean...",arguments = list(1, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      f1STD<-as.numeric(praat("Get standard deviation...",arguments = list(1, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      fmean2<-as.numeric(praat("Get mean...",arguments = list(2, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      f2STD<-as.numeric(praat("Get standard deviation...",arguments = list(2, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      fmean3<-as.numeric(praat("Get mean...",arguments = list(3, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      f3STD<-as.numeric(praat("Get standard deviation...",arguments = list(3, 0, 0, "Hertz"),input = FullPath("4formant"),simplify = TRUE))
      f2meanf1<-as.numeric(fmean2)/as.numeric(fmean1)
      f3meanf1<-as.numeric(fmean3)/as.numeric(fmean1)
      f2STDf1<-as.numeric(f2STD)/as.numeric(f1STD)
      f3STDf1<-as.numeric(f3STD)/as.numeric(f1STD)

      #Jitter
      jitter<-as.numeric(praat("Get jitter (local)...",arguments = list(0, 0, 0.0001, 0.02, 1.3),input = FullPath("4pointprocess"),simplify = TRUE))
      jitterRap<-as.numeric(praat("Get jitter (rap)...",arguments = list(0, 0, 0.0001, 0.02, 1.3),input = FullPath("4pointprocess"),simplify = TRUE))

      #shimmer
      shimmer<-as.numeric(praat("Get shimmer (local)...",arguments = list(0.0001, 0.02, 1.6),input = FullPath("4amplitudetier"),simplify = TRUE))
      meanPeriod<-as.numeric(praat("Get mean period...",arguments = list(0, 0, 0.0001, 0.02, 1.3),input = FullPath("4pointprocess"),simplify = TRUE))
      
      #extracted_y <- read.csv(file=FullPath("y.csv"), header = T, stringsAsFactors = F)
      extracted_y<-data.frame(duration,energy,power,min_pitch,max_pitch,mean_pitch,pitch_sd,pitch_quant,diffPitchMaxMin,diffPitchMaxMean,intensityMin,intensityMax,intensityMean,intensitySD,intensityQuant,diffIntMaxMin,diffIntMaxMean,avgVal1,avgVal2,avgVal3,avgBand1,avgBand2,avgBand3,fmean1,fmean2,fmean3,f2meanf1,f3meanf1,f1STD,f2STD,f3STD,f2STDf1,f3STDf1,jitter,shimmer,jitterRap,meanPeriod,percentUnvoiced)
      extracted_y<- data.frame(extracted_y)
      write.csv(extracted_y, FullPath('Extracted.csv'))
      
      temp_extract=1
      return(temp_extract)
      
    })
  extraction<-reactive({
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    while(extraction2()==0) wait()
    extracted_y2<- read.csv(file=FullPath("Extracted.csv"), header = T, stringsAsFactors = F)
    extracted_y2<- data.frame(extracted_y2)
    return(extracted_y2)
    })
  y_confidence<-reactive({
    
    final_confidence =c("duration", "energy","power", "max_pitch", "mean_pitch", "pitch_sd",
                        "pitch_quant",  "diffPitchMaxMin",
                        "diffPitchMaxMean", "intensityMin", "intensityMax", "intensityMean",
                        "intensitySD", "intensityQuant", "diffIntMaxMin", "avgVal1", "avgVal2",
                        "avgVal3", "avgBand1", "avgBand2", "avgBand3", "fmean1",
                        "fmean2" , "fmean3", "f2meanf1" ,        "f3meanf1" ,   "f1STD" ,
                        "f2STD" , "f3STD"   , "f2STDf1"   ,       "f3STDf1"   ,  "jitter",
                        "shimmer" , "jitterRap",  "meanPeriod", "percentUnvoiced" )
    extracted_y_conf<-subset(extraction(), select= c(final_confidence))
    return(extracted_y_conf)
  })
  
  y_fluency<-reactive({
    final_fluency= c("duration","energy","power","mean_pitch","pitch_sd","pitch_quant","intensityMin","intensityMax","intensityMean","intensitySD","intensityQuant","diffIntMaxMin","avgVal1","avgVal2","avgVal3","avgBand1","avgBand2","avgBand3","fmean1","fmean2","fmean3","f2meanf1","f3meanf1","f1STD","f2STD","f3STD","f2STDf1","jitter","shimmer","jitterRap","meanPeriod","percentUnvoiced" )
    extracted_y_flu<-subset(extraction(), select= c(final_fluency))
    #extracted_y_flu<- data.frame(duration,energy,power,mean_pitch,pitch_sd,pitch_quant,diffPitchMaxMin,diffPitchMaxMean,intensityMin,intensityMax,intensityMean,intensitySD,intensityQuant,diffIntMaxMin,diffIntMaxMean,avgVal1,avgVal2,avgVal3,avgBand1,avgBand2,avgBand3,fmean1,fmean2,fmean3,f2meanf1,f3meanf1,f1STD,f2STD,f3STD,f2STDf1,jitter,shimmer,jitterRap,meanPeriod,percentUnvoiced)
    return(extracted_y_flu)
  })
  y_energetic<-reactive({
    final_energetic = c( "duration" ,        "energy"  ,         "power"  ,          "max_pitch"  ,      "mean_pitch",
                         "pitch_sd" ,                "pitch_quant",
                         "diffPitchMaxMin",  "diffPitchMaxMean", "intensityMin"  ,   "intensityMax"  ,
                         "intensityMean",    "intensitySD",      "intensityQuant",   "diffIntMaxMin" ,
                         "avgVal1" ,         "avgVal2",   "avgVal3",             "avgBand1"  ,       "avgBand2"  ,
                         "avgBand3"  ,       "fmean1"   ,       "fmean2"   , "fmean3"  ,  "f2meanf1" ,        "f3meanf1"    ,
                         "f1STD"     ,       "f2STD"    ,        "f3STD"     ,       "f2STDf1"  ,        "f3STDf1"     ,
                         "jitter"     ,      "shimmer"   ,       "jitterRap"  ,      "meanPeriod",       "percentUnvoiced")
    extracted_y_ener<-subset(extraction(), select= c(final_energetic))
    #extracted_y_ener<-data.frame(duration,energy,power,max_pitch,mean_pitch,pitch_sd,pitch_quant,diffPitchMaxMin,diffPitchMaxMean,intensityMin,intensityMax,intensityMean,intensitySD,intensityQuant,diffIntMaxMin,avgVal1,avgVal2,avgVal3,avgBand1,avgBand2,avgBand3,fmean1,fmean2,fmean3,f2meanf1,f3meanf1,f1STD,f2STD,f3STD,f2STDf1,f3STDf1,jitter,shimmer,jitterRap,meanPeriod,percentUnvoiced)
    
    return(extracted_y_ener)
  })
  y_clarity<-reactive({
    final_clarity = c(      "energy" ,          "power"   ,  "max_pitch",       "mean_pitch" ,      "pitch_sd"  ,
                            "pitch_quant",    "diffPitchMaxMean",
                            "intensityMin",     "intensityMax",     "intensityMean" ,   "intensitySD",      "intensityQuant" ,
                            "diffIntMaxMin", "diffIntMaxMean",  "avgVal1"  ,        "avgVal2"  ,        "avgVal3"    ,      "avgBand1"  ,
                            "avgBand2" ,        "avgBand3"  ,       "fmean1"   ,   "fmean2",     "fmean3"      ,     "f2meanf1"        ,
                            "f3meanf1"  ,       "f1STD"      ,      "f2STD"     ,       "f3STD"        ,    "f2STDf1"         ,
                            "f3STDf1"      ,    "jitter"        ,   "shimmer"      ,    "jitterRap"       , "meanPeriod"      ,
                            "percentUnvoiced" )
    extracted_y_clar<-subset(extraction(), select= c(final_clarity))
    #extracted_y_clar<-data.frame(energy,power,max_pitch,mean_pitch,pitch_sd,pitch_quant,diffPitchMaxMean,intensityMin,intensityMax,intensityMean,intensitySD,intensityQuant,diffIntMaxMin,diffIntMaxMean,avgVal1,avgVal2,avgVal3,avgBand1,avgBand2,avgBand3,fmean1,fmean2,fmean3,f2meanf1,f3meanf1,f1STD,f2STD,f3STD,f2STDf1,f3STDf1,jitter,shimmer,jitterRap,meanPeriod,percentUnvoiced)
    return(extracted_y_clar)
    
  })


  output$svm<-renderTable(
    {
      library(xlsx)
      extracted_y_conf<-y_confidence()
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep="") ) }
      svm_model_after_tune_c<-readRDS(FullPath("svm_model_after_tune_c.rds"))
      confidence_pred <- predict(svm_model_after_tune_c,extracted_y_conf)
      Confidence <- as.numeric(confidence_pred)
      
      # # #################Fluency#############
      extracted_y_flu<-y_fluency()
      svm_model_after_tune_f<-readRDS(FullPath("svm_model_after_tune_f.rds"))
      fluency_pred <- predict(svm_model_after_tune_f,extracted_y_flu)
      Fluency <- as.numeric(fluency_pred)
      
      # ##########Energetic Model###########
      #
      extracted_y_ener<-y_energetic()
      svm_model_after_tune_e<-readRDS(FullPath("svm_model_after_tune_e.rds"))
      energetic_pred <- predict(svm_model_after_tune_e,extracted_y_ener)
      Energetic <- as.numeric(energetic_pred)
      
      #
      # ##########Clarity Model###########
      #
      extracted_y_clar<-y_clarity()
      svm_model_after_tune_cl<-readRDS(FullPath("svm_model_after_tune_cl.rds"))
      clarity_pred <- predict(svm_model_after_tune_cl,extracted_y_clar)
      Clarity <- as.numeric(clarity_pred)
      
      models<-data.frame(Confidence,Fluency,Energetic,Clarity)
      Confidence<-(Confidence*10)/3
      Fluency<-(Fluency*10)/3
      Energetic<-(Energetic*10)/3
      Clarity<-(Clarity*10)/3
      # oi_data <- read.csv(file=FullPath("oi.csv"), header = T, stringsAsFactors = F)
      # oi_data<- data.frame(oi_data)
      # x_oi <- subset(oi_data, select= - oration_index)
      # y_oi <- subset(oi_data, select = oration_index)
      # svm_model_oi <-svm(x_oi,y_oi)
      # svm_tune <- tune(svm, train.x=x_oi, train.y=y_oi, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
      # svm_model_after_tune <- svm(oi_data$oration_index ~ ., data=oi_data, kernel="radial")
      svm_model_after_tune<-readRDS(FullPath("svm_model_after_tune_oi.rds"))
      oi_pred <- predict(svm_model_after_tune,models)
      oi_pred <- as.numeric(oi_pred)
      #oi_pred<-(oi_pred*10)/3
      oi_pred <- data.frame(oi_pred)
      #
      attri <- data.frame(Confidence, Fluency, Energetic,Clarity,oi_pred)
      svm_result<-data.frame(Confidence,Fluency,Energetic,Clarity)
      write.csv(svm_result,FullPath("Svm_result.csv"))
      #write.csv(new.data, FullPath('Data.csv'))

      return(attri)
    }
  )
  output$svm_plot<-renderPlot({
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    svm_result<-data.frame(read.csv(file=FullPath("Svm_result.csv"),header = T,stringsAsFactors = F))
    svm_result<-subset(svm_result, select= c('Confidence','Fluency','Energetic','Clarity'))
    Confidence1<-svm_result$Confidence
    Fluency1<-svm_result$Fluency
    Energetic1<-svm_result$Energetic
    Clarity1<-svm_result$Clarity
    attri<-data.frame(Confidence1,Fluency1,Energetic1,Clarity1)
    attri=rbind(rep(10,4) , rep(0,4) , attri)
    #print(radarchart(attri))
    return(radarchart( attri  , axistype=1 ,

                      #custom polygon
                      pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,

                      #custom the grid
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10), cglwd=0.8,

                      #custom labels
                      vlcex=0.8
    ))
  })
  name<-reactive({return(input$Name)})
  jobprof<-reactive({
    if(input$jobprof=="Developer"){temp=5}
    if(input$jobprof=="Analyst"){temp=7}
    if(input$jobprof=="Administrator"){temp=6}
    return(temp)})
  gender1<-reactive({return(input$gender)})
  output$temp<-renderTable({return(extraction())})

  output$ann<-renderTable(
    {
      # ##########Confidence#############
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep="") ) }
      to_predict <- y_confidence()
      to_predict[-2] <- lapply(to_predict[-2], function(x) as.numeric(sub("\\s+\\D+$", "", x)))
      net_c<-readRDS(FullPath("net_c.rds"))
      j <- compute(net_c, to_predict)$net.result

      low_confident <- j[1,1]
      med_confident <- j[1,2]
      High_confident <- j[1,3]
      Confidence <- data.frame(low_confident,med_confident,High_confident)
      if(max(Confidence)==low_confident){
        Confidence_result=1 
        Confidence_nom="Low"
      }
      if(max(Confidence)==med_confident){
        Confidence_result=2
        Confidence_nom="Medium"
      
      }
      if(max(Confidence)==High_confident){
        Confidence_result=3
        Confidence_nom="High"
        }

      # ###################Fluency####################

     #  fluency_data <- read.csv(file=FullPath("fluency.csv"), header = T, stringsAsFactors = F)
      to_predict_fluency1 <- y_fluency()
       to_predict_fluency1[-2] <- lapply(to_predict_fluency1[-2], function(x) as.numeric(sub("\\s+\\D+$", "", x)))
 
     net_f<-readRDS(FullPath("net_f.rds")) 
     f <- compute(net_f, to_predict_fluency1)$net.result

      low_fluent <- f[1,1]
      med_fluent <- f[1,2]
      High_fluent <- f[1,3]

      Fluency <- data.frame(low_fluent,med_fluent,High_fluent)
      if(max(Fluency)==low_fluent){
        Fluency_result=1
        Fluency_nom="Low"}
      if(max(Fluency)==med_fluent){
        Fluency_result=2
        Fluency_nom="Medium"}
      if(max(Fluency)==High_fluent){
        Fluency_result=3
        Fluency_nom="High"}

      ##################Energetic##################

      #energetic_data <- read.csv(file=FullPath("energetic.csv"), header = T, stringsAsFactors = F)
      to_predict_energetic1 <- y_energetic()
      to_predict_energetic1[-2] <- lapply(to_predict_energetic1[-2], function(x) as.numeric(sub("\\s+\\D+$", "", x)))
   
      net_e<-readRDS(FullPath("net_e.rds"))
      e <- compute(net_e, to_predict_energetic1)$net.result

      low_energetic <- e[1,1]
      med_energetic <- e[1,2]
      High_energetic <- e[1,3]
      Energetic <- data.frame(low_energetic,med_energetic,High_energetic)
      if(max(Energetic)==low_energetic){
        Energetic_result=1
        Energetic_nom="Low"
        }
      if(max(Energetic)==med_energetic){
        Energetic_result=2
        Energetic_nom="Medium"
        }

      if(max(Energetic)==High_energetic){
        Energetic_result=3
        Energetic_nom="High"}

      ###################Clarity####################


     #  clarity_data <- read.csv(file=FullPath("clarity.csv"), header = T, stringsAsFactors = F)
      to_predict_clarity <- y_clarity()
       to_predict_clarity[-2] <- lapply(to_predict_clarity[-2], function(x) as.numeric(sub("\\s+\\D+$", "", x)))
    net_cl<-readRDS(FullPath("net_cl.rds"))  
     cl <- compute(net_cl, to_predict_clarity)$net.result

      low_clear <- cl[1,1]
      med_clear <- cl[1,2]
      High_clear <- cl[1,3]
      Clarity <- data.frame(low_clear,med_clear,High_clear)
      if(max(Clarity)==low_clear){
        Clarity_result=1
      Clarity_nom="Low"}
      if(max(Clarity)==med_clear){
        Clarity_result=2
        Clarity_nom="Medium"}
      if(max(Clarity)==High_clear){
        Clarity_result=3
        Clarity_nom="High"}

      ann_result<-data.frame(Confidence_nom,Fluency_nom,Energetic_nom,Clarity_nom)
      ann_result<-data.frame(Confidence_result,Energetic_result,Fluency_result,Clarity_result)
      write.csv(ann_result,FullPath("Ann_result.csv"),row.names = F)

      return(ann_result)


    }
  )
  output$ann_plot<-renderPlot(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      ann_result<-data.frame(read.csv(file=FullPath("Ann_result.csv"),header = T,stringsAsFactors = F))
      ann_result<-subset(ann_result, select= c('Confidence_result','Fluency_result','Energetic_result','Clarity_result'))
      Confidence1<-ann_result$Confidence_result
      Fluency1<-ann_result$Fluency_result
      Energetic1<-ann_result$Energetic_result
      Clarity1<-ann_result$Clarity_result
      attri<-as.matrix(data.frame(Confidence1,Fluency1,Energetic1,Clarity1))
      return(barplot(attri,ylim=c(0,3),col=c("cyan","coral2","darkolivegreen2","cadetblue2"),names.arg=c("Confidence","Fluency","Energetic","Clarity")))
    }
  )

  output$all_questions_conf<-renderPlot({
    library(ggplot2)
    
    attach(mtcars)
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_oration_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
    c=0
    conf_data<-c(1:6)
    for(x in c(1:nrow(interviewee_data)))
    {
      if(interviewee_data[x,2]==name()){

        c=c+1
        conf_data[c]=as.numeric(interviewee_data[x,5])

      }

    }
    return(barplot(conf_data))
  })
  output$all_questions_flu<-renderPlot({

    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_oration_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
    c=0
    flu_data<-c(1:6)
    for(x in c(1:nrow(interviewee_data)))
    {
      if(interviewee_data[x,2]=="Vineet"){

        c=c+1
        flu_data[c]=as.numeric(interviewee_data[x,6])

      }

    }
    return(barplot(flu_data))
  })
  output$all_questions_ener<-renderPlot({

    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_oration_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
    c=0
    ener_data<-c(1:6)
    for(x in c(1:nrow(interviewee_data)))
    {
      if(interviewee_data[x,2]=="Vineet"){

        c=c+1
        ener_data[c]=as.numeric(interviewee_data[x,7])

      }

    }
    return(barplot(ener_data))
  })
  output$all_questions_clar<-renderPlot({

    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_oration_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
    c=0
    clar_data<-c(1:6)
    for(x in c(1:nrow(interviewee_data)))
    {
      if(interviewee_data[x,2]=="Vineet"){

        c=c+1
        clar_data[c]=as.numeric(interviewee_data[x,8])

      }

    }
    return(barplot(clar_data))
  })
  output$all_interviewee_conf<-renderPlot(
    {
      library(ggplot2)
      attach(mtcars)
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_oration_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
      ux<-unique(interviewee_data[,2])
      n<-list()
      z=1
      sum_conf<-list()
      for(y in ux){
        #print(z)
        sum_conf[z]<-0
        n[z]<-0
        for(x in c(1:nrow(interviewee_data)))
        {
          
          if(interviewee_data[x,2]==y){
            sum_conf[z]=as.numeric(sum_conf[z])+as.numeric(interviewee_data[x,5])
            n[z]=as.numeric(n[z])+1
           
          }
          
        }
        #print("count of "+y+"="+n[z])
        sum_conf[z]<-as.numeric(as.numeric(sum_conf[z])/as.numeric(n[z]))
        
        z=z+1
      }
      
      conf<-as.numeric(sum_conf)
      return(barplot(conf,names.arg = ux))
      
      
    }
  )
  output$all_interviewee_flu<-renderPlot(
    {
      library(ggplot2)
      attach(mtcars)
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      interviewee_data<-data.frame(read.csv(file=FullPath("Data2.csv"),header = T,stringsAsFactors = F))
      ux<-unique(interviewee_data[,2])
      
      
      
      n<-list()
     
      z=1
      
      sum_flu<-list()
      sum_ener<-list()
      sum_clar<-list()
      
      for(y in ux){
      
        
        sum_flu[z]<-0
        
        n[z]<-0
        for(x in c(1:nrow(interviewee_data)))
        {
          
          if(interviewee_data[x,2]==y){
            
            sum_flu[z]=as.numeric(sum_flu[z])+as.numeric(interviewee_data[x,6])
            
            n[z]=as.numeric(n[z])+1
            
          }
          
        }
        #print("count of "+y+"="+n[z])
        
        sum_flu[z]<-as.numeric(as.numeric(sum_flu[z])/as.numeric(n[z]))
       
        z=z+1
      }
      
      conf<-as.numeric(sum_flu)
      return(barplot(conf,names.arg = ux))
      
      
    }
  )
  output$all_interviewee_ener<-renderPlot(
    {
      library(ggplot2)
      attach(mtcars)
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      interviewee_data<-data.frame(read.csv(file=FullPath("Data2.csv"),header = T,stringsAsFactors = F))
      ux<-unique(interviewee_data[,2])
      n<-list()
      z=1
      sum_ener<-list()
      
      
      for(y in ux){
       
        
        
        sum_ener[z]<-0
        
        n[z]<-0
        for(x in c(1:nrow(interviewee_data)))
        {
          
          if(interviewee_data[x,2]==y){
            
           
            sum_ener[z]=as.numeric(sum_ener[z])+as.numeric(interviewee_data[x,7])
            
            n[z]=as.numeric(n[z])+1
           
          }
          
        }
        #print("count of "+y+"="+n[z])
        
        
        sum_ener[z]<-as.numeric(as.numeric(sum_ener[z])/as.numeric(n[z]))
        
        z=z+1
      }
      
      conf<-as.numeric(sum_ener)
      return(barplot(conf,names.arg = ux))
    }
  )
  output$all_interviewee_clar<-renderPlot(
    {
      library(ggplot2)
      attach(mtcars)
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      interviewee_data<-data.frame(read.csv(file=FullPath("Data2.csv"),header = T,stringsAsFactors = F))
      ux<-unique(interviewee_data[,2])
      
      
      
      n<-list()
      
      z=1
      
     
      sum_clar<-list()
      
      for(y in ux){
       
        
        sum_clar[z]<-0
        n[z]<-0
        for(x in c(1:nrow(interviewee_data)))
        {
          
          if(interviewee_data[x,2]==y){
            
            
            sum_clar[z]=as.numeric(sum_clar[z])+as.numeric(interviewee_data[x,8])
            n[z]=as.numeric(n[z])+1
           
          }
          
        }
        #print("count of "+y+"="+n[z])
        
        sum_clar[z]<-as.numeric(as.numeric(sum_clar[z])/as.numeric(n[z]))
        z=z+1
      }
      
      conf<-as.numeric(sum_clar)
      return(barplot(conf,names.arg = ux))
    }
  )
 
  
  output$randomforest_results<-renderTable(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
      
      randomforest_model_confidence<-readRDS(FullPath("rf_c.rds")) 
      pred_confidence<-as.numeric(predict(randomforest_model_confidence,newdata=y_confidence()))
     
      
      randomforest_model_fluency<-readRDS(FullPath("rf_f.rds")) 
      pred_fluency<-as.numeric(predict(randomforest_model_fluency,newdata=y_fluency()))
      
      
      
      randomforest_model_energetic<-readRDS(FullPath("rf_e.rds")) 
      pred_energetic<-as.numeric(predict(randomforest_model_energetic,newdata=y_energetic()))
      
      randomforest_model_clarity<-readRDS(FullPath("rf_cl.rds")) 
      pred_clarity<-as.numeric(predict(randomforest_model_clarity,newdata=y_clarity()))
     
      rf_result<-data.frame(pred_confidence,pred_fluency,pred_energetic,pred_clarity)
       randomforest_model_oi<-readRDS(FullPath("rf_oi.rds")) 
       Oration_index<-predict(randomforest_model_oi,newdata=rf_result)
      
      pred_confidence<-(pred_confidence*10)/3
      Confidence<-round(pred_confidence)
      pred_fluency<-(pred_fluency*10)/3
      Fluency<-round(pred_fluency)
      pred_energetic<-(pred_energetic*10)/3
      Energetic<-round(pred_energetic)
      pred_clarity<-(pred_clarity*10)/3
      Clarity<-round(pred_clarity)
      rf_result<-data.frame(Confidence,Fluency,Energetic,Clarity)
      data_to_be_added<-data.frame(Sys.time(),name(),input$Rec_no, gender1(),Confidence,Fluency,Energetic,Clarity)
      SHEET.NAME <- 'Sheet1'
      file<-FullPath("Data_oration_analysis.xlsx")
      existing.data <- read.xlsx(file, sheetName = SHEET.NAME)
      new.data <- rbind(existing.data, data_to_be_added)
      write.xlsx(new.data, file, sheetName = SHEET.NAME, row.names = F, append = F)
      write.csv(rf_result,FullPath("Rf_result.csv"),row.names = F)
      return(as.data.frame(rf_result))
    }
  )
  output$confidence<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("Rf_result.csv"),header = T,stringsAsFactors = F))
    rf_result<-subset(rf_result, select= c('Confidence','Fluency','Energetic','Clarity'))
    Confidence1<-floor(rf_result$Confidence)
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Confidence",
      color = "teal"
    )
  })
  output$fluency<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("Rf_result.csv"),header = T,stringsAsFactors = F))
    rf_result<-subset(rf_result, select= c('Confidence','Fluency','Energetic','Clarity'))
    Confidence1<-floor(rf_result$Fluency)
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Fluency",
      color = "olive"
    )
  })
  output$energetic<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("Rf_result.csv"),header = T,stringsAsFactors = F))
    rf_result<-subset(rf_result, select= c('Confidence','Fluency','Energetic','Clarity'))
    Confidence1<-floor(rf_result$Energetic)
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Energetic",
      color = "aqua"
    )
  })
  output$clarity<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/finalappmit/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("Rf_result.csv"),header = T,stringsAsFactors = F))
    rf_result<-subset(rf_result, select= c('Confidence','Fluency','Energetic','Clarity'))
    Confidence1<-floor(rf_result$Clarity)
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Clarity",
      color = "teal"
    )
  })
  
  ##function for donot plottin
  doughnut <-
    function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
              inner.radius=0.6, clockwise = FALSE,
              init.angle = if (clockwise) 90 else 0, density = NULL, 
              angle = 45, col = NULL, border = FALSE, lty = NULL, 
              main = NULL, ...)
    {
      if (!is.numeric(x) || any(is.na(x) | x < 0))
        stop("'x' values must be positive.")
      if (is.null(labels))
        labels <- as.character(seq_along(x))
      else labels <- as.graphicsAnnot(labels)
      x <- c(0, cumsum(x)/sum(x))
      dx <- diff(x)
      nx <- length(dx)
      plot.new()
      pin <- par("pin")
      xlim <- ylim <- c(-1, 1)
      if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
      else ylim <- (pin[2L]/pin[1L]) * ylim
      plot.window(xlim, ylim, "", asp = 1)
      if (is.null(col))
        col <- if (is.null(density))
          palette()
      else par("fg")
      col <- rep(col, length.out = nx)
      border <- rep(border, length.out = nx)
      lty <- rep(lty, length.out = nx)
      angle <- rep(angle, length.out = nx)
      density <- rep(density, length.out = nx)
      twopi <- if (clockwise)
        -2 * pi
      else 2 * pi
      t2xy <- function(t, radius) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), 
             y = radius * sin(t2p))
      }
      for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                  outer.radius)
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
                angle = angle[i], border = border[i], 
                col = col[i], lty = lty[i])
        Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
          lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
          text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
               xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
               ...)
        }
        ## Add white disc          
        Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                    inner.radius)
        polygon(Pin$x, Pin$y, density = density[i], 
                angle = angle[i], border = border[i], 
                col = "white", lty = lty[i])
      }
      
      title(main = main, ...)
      invisible(NULL)
    }
  
  output$resume_developer<-renderPlot(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      #Adding the new resume
      curr_resume<-readChar(data_resume(), file.info(data_resume())$size)
      a7<-"as"
      a7<-append(a7,curr_resume)
      data.raw<-read.csv(file=FullPath("software_dev.csv"),header = T,stringsAsFactors = F)
      
      software_dev<-c("software" , "project" , "developer" , "web" , "experience" , "sql" , "end" , "php" , "developed" , "java" , "mysql" , "css" , "server" , "development" , "html" , "years" , "module" , "management" , "asp.net" , "system" , "c#" , "data" , "laravel" , "application" , "front" , "api" , "online" , "technical" , "bootstrap" , "design" , "website" , "programming" , "admin" , "database" , "windows" , "ms" , "responsibilities" , "c" , "projects" , "back" , "responsible" , "android" , "html5" , "json" , "languages" , "tools" , "systems" , "core" , "operating" , "implemented" , "master" , "role" , "rest" , "codeigniter" , "net" , "designing" , "gives" , "learn" , "thane" , "xml" , "user" , "dynamic" , "service" , "organization" , "microsoft" , "learning" , "merchant" , "professional" , "infotech" , "content" , "company" , "oracle" , "technologies" , "testing" , "done" , "business" , "engineer" , "involved" , "ui" , "creating" , "platform" , "responsive" , "apache" , "implementation" , "main" , "barcode" , "r2" , "provides" , "self" , "india" , "functionality" , "validation" , "level" , "soap" , "datapower" , "integration" , "environment" , "eclipse" , "netbeans" , "control" , "excel" , "word" , "implementing", "technology","learning","training","code","coding","model","java","bootstrap","php","swing","project","data","warehouse","database","execution","hackathon","competition")
      software_dev<-unique(software_dev)
      analyst_words<-c("skills" ,"modelling","analysis","analyst","analysed","preprocessing","processing","xml","ad-hoc","testing", "year" , "work" , "less" , "experience" , "analyst" , "management" , "project" , "analysis" , "financial" , "good" , "software" , "finance" , "services" , "module" , "information" , "data" , "business" , "level" , "excel" , "development" , "research" , "training" , "time" , "award" , "market" , "applications" , "analytics" , "corporate" , "sector" , "industry" , "preparing" , "organization" , "learning" , "technical" , "ncfm" , "modeling" , "positive" , "advanced" , "certification" , "office" , "learner" , "datapower" , "professional" , "engineering" , "building" , "clients" , "operations" , "key" , "results" , "model" , "developing" , "client" , "service" , "december" , "credit" , "reports" , "cfa" , "projects" , "accounting" , "database" , "report" , "consensus" , "statement" , "name" , "life" , "various" , "study" ,  "investor" , "diploma" , "markets" , "beginners" , "ability" , "crisil" , "assessing" , "grading")
      admin_words<-c("information" , "management" , "administrator" , "years" , "communication" , "additional" , "new" , "working" , "handling" , "services" , "learning" , "good" , "staff" , "ltd" , "knowledge" , "team" , "records" , "excel" , "help" , "maintenance" , "time" , "diploma" , "software" , "months" , "preparing" , "data" , "reports" , "prepare" , "administrative" , "computer" , "strong" , "able" , "professional" , "june" , "period" , "customer" , "calls" , "organization" , "record" , "production" , "technical" , "datapower" , "project" , "plans" , "environment" , "attitude" , "ability" , "results" , "nature" , "people" , "committed" , "manager" , "quality" , "hr" , "designation" , "current" , "systems" , "travel" , "arrangements" , "effective" , "database" , "finance" , "correspondence" , "technology" , "procedures" , "service" , "testing" , "test" , "field" , "high" , "excellent" , "personnel" , "policies" , "organize" , "general" , "administration" , "needs" , "leading" , "effectively" , "ensuring" , "social" , "existing" , "dedicated" , "handle" , "related" , "problems" , "admin" , "major" , "responsibility" , "state" , "learner" , "level" , "oriented" , "challenging" , "business" , "documents" , "files" , "schedule" , "company" , "equipment" , "employees" , "processes" , "dynamic" , "solution" , "word" , "benefits" , "compensation" , "employee" , "performance" , "preparation" , "establish" , "utilize" , "achieve" , "procurement", "implementation","team","teamwork","logistics","finance","handled","manage","scheduling")
      
      data.raw <- rbind(data.raw, a7)
      data.raw<-data.raw[!apply(data.raw == "", 1, all),]
      
      library(caret)
      train<-data.raw
      library(quanteda)
      train.tokens1 <- tokens(train$resume, what = "word", 
                              remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = TRUE, remove_hyphens = TRUE)
      train.tokens <- tokens_tolower(train.tokens1)
      train.tokens <- tokens_select(train.tokens, stopwords(), 
                                    selection = "remove")
      #train.tokens <- tokens_wordstem(train.tokens, language = "english")
      #names(train.tokens) <- make.names(names(train.tokens))
      train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
      train.tokens.matrix <- as.matrix(train.tokens.dfm)
      software_dev2<-c()
      for(x in 1:length(software_dev))
      {
        if(! is.na(match(software_dev[x],colnames(train.tokens.matrix))))
          software_dev2<-append(software_dev[x],software_dev2)
        
      }
      train.tokens.matrix<-subset(train.tokens.matrix,select=software_dev2)
      
      term.frequency <- function(row) {
        row / sum(row)
      }
      
      inverse.doc.freq <- function(col) {
        corpus.size <- length(col)
        doc.count <- length(which(col > 0))
        
        log10(corpus.size / doc.count)
      }
      
      tf.idf <- function(x, idf) {
        x * idf
      }
      
      train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
      #train.tokens.df <- train.tokens.df[!apply(is.na(train.tokens.df) | train.tokens.df == "", 1, all),]
      train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
      str(train.tokens.idf)
      train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
      train.tokens.tfidf <- t(train.tokens.tfidf)
      m<-as.matrix(train.tokens.tfidf)
      m2<-t(m)
      rownames(m)<-1:nrow(m)
      norm_eucl<-function(m)
        m/apply(m,1,function(x) sum(x^2)^.5)
      
      m_norm<-norm_eucl(m)
      #m_norm<-t(m_norm)
      results<-kmeans(m_norm,3,10)
      
      rhs<-results$cluster
      weights <- relief(rhs~., as.data.frame(m_norm),neighbours.count = 3)
      weights<-abs(weights)
      cbr<-c(1:nrow(m_norm))
      for(x in 1:nrow(m_norm))
      {
        cbr[x]<-0
        for(y in 1:ncol(m_norm))
        {
          prod=weights[y,1]*m_norm[x,y]
          cbr[x]=cbr[x]+prod
        }
      }
      cbr<-cbr*10000
      
      rating_dev<-(match(cbr[nrow(m_norm)],sort(cbr))*10)/length(as.matrix(cbr))
      doughnut <-
        function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
                  inner.radius=0.6, clockwise = FALSE,
                  init.angle = if (clockwise) 90 else 0, density = NULL, 
                  angle = 45, col = NULL, border = FALSE, lty = NULL, 
                  main = NULL, ...)
        {
          if (!is.numeric(x) || any(is.na(x) | x < 0))
            stop("'x' values must be positive.")
          if (is.null(labels))
            labels <- as.character(seq_along(x))
          else labels <- as.graphicsAnnot(labels)
          x <- c(0, cumsum(x)/sum(x))
          dx <- diff(x)
          nx <- length(dx)
          plot.new()
          pin <- par("pin")
          xlim <- ylim <- c(-1, 1)
          if (pin[1L] > pin[2L])
            xlim <- (pin[1L]/pin[2L]) * xlim
          else ylim <- (pin[2L]/pin[1L]) * ylim
          plot.window(xlim, ylim, "", asp = 1)
          if (is.null(col))
            col <- if (is.null(density))
              palette()
          else par("fg")
          col <- rep(col, length.out = nx)
          border <- rep(border, length.out = nx)
          lty <- rep(lty, length.out = nx)
          angle <- rep(angle, length.out = nx)
          density <- rep(density, length.out = nx)
          twopi <- if (clockwise)
            -2 * pi
          else 2 * pi
          t2xy <- function(t, radius) {
            t2p <- twopi * t + init.angle * pi/180
            list(x = radius * cos(t2p), 
                 y = radius * sin(t2p))
          }
          for (i in 1L:nx) {
            n <- max(2, floor(edges * dx[i]))
            P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                      outer.radius)
            polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
                    angle = angle[i], border = border[i], 
                    col = col[i], lty = lty[i])
            Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
            lab <- as.character(labels[i])
            if (!is.na(lab) && nzchar(lab)) {
              lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
              text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
                   xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
                   ...)
            }
            ## Add white disc          
            Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                        inner.radius)
            polygon(Pin$x, Pin$y, density = density[i], 
                    angle = angle[i], border = border[i], 
                    col = "white", lty = lty[i])
          }
          
          title(main = main, ...)
          invisible(NULL)
        }
      rating_dev<-abs(rating_dev)
      print(rating_dev)
      write.csv(rating_dev,file = FullPath("rating_dev.csv"))
      top10<-tail(sort(m[nrow(m),]),5)
      toMatch<-names(top10)
      sentences<-tolower(unlist(strsplit(curr_resume,split="\\.")))
      sentences<-sentences[which(nchar(sentences)>40)]
      foo<-function(Match){sentences[grep(Match,sentences)]}
      jist<-unique(lapply(toMatch,foo))
      jist_list<-list()
      for(x in c(1:length(jist))){
        for(y in c(1:length(jist[[x]]))){
          jist_list<-append(jist_list,jist[[x]][y])}
      }
      imp_sentences<-unique(jist_list)
      write.table(imp_sentences,FullPath("resume_imp_sentences.txt"))
      plot_dev<-doughnut(c(rating_dev,10-rating_dev) ,labels=c("Development","."),init.angle = 0, col=c("cadetblue3","cadetblue1") )
      return(plot_dev)
    }
  )
  
  output$resume_admin<-renderPlot(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      
      #Adding the new resume
      curr_resume<-readChar(data_resume(), file.info(data_resume())$size)
      a7<-"as"
      a7<-append(a7,curr_resume)
      data.raw<-read.csv(file=FullPath("admin.csv"),header = T,stringsAsFactors = F)
      
      software_dev<-c("software" , "project" , "developer" , "web" , "experience" , "sql" , "end" , "php" , "developed" , "java" , "mysql" , "css" , "server" , "development" , "html" , "years" , "module" , "management" , "asp.net" , "system" , "c#" , "data" , "laravel" , "application" , "front" , "api" , "online" , "technical" , "bootstrap" , "design" , "website" , "programming" , "admin" , "database" , "windows" , "ms" , "responsibilities" , "c" , "projects" , "back" , "responsible" , "android" , "html5" , "json" , "languages" , "tools" , "systems" , "core" , "operating" , "implemented" , "master" , "role" , "rest" , "codeigniter" , "net" , "designing" , "gives" , "learn" , "thane" , "xml" , "user" , "dynamic" , "service" , "organization" , "microsoft" , "learning" , "merchant" , "professional" , "infotech" , "content" , "company" , "oracle" , "technologies" , "testing" , "done" , "business" , "engineer" , "involved" , "ui" , "creating" , "platform" , "responsive" , "apache" , "implementation" , "main" , "barcode" , "r2" , "provides" , "self" , "india" , "functionality" , "validation" , "level" , "soap" , "datapower" , "integration" , "environment" , "eclipse" , "netbeans" , "control" , "excel" , "word" , "implementing" )
      admin_words<-c("information" , "management" , "administrator" , "years" , "communication" , "additional" , "new" , "working" , "handling" , "services" , "learning" , "good" , "staff" , "ltd" , "knowledge" , "team" , "records" , "excel" , "help" , "maintenance" , "time" , "diploma" , "software" , "months" , "preparing" , "data" , "reports" , "prepare" , "administrative" , "computer" , "strong" , "able" , "professional" , "june" , "period" , "customer" , "calls" , "organization" , "record" , "production" , "technical" , "datapower" , "project" , "plans" , "environment" , "attitude" , "ability" , "results" , "nature" , "people" , "committed" , "manager" , "quality" , "hr" , "designation" , "current" , "systems" , "travel" , "arrangements" , "effective" , "database" , "finance" , "correspondence" , "technology" , "procedures" , "service" , "testing" , "test" , "field" , "high" , "excellent" , "personnel" , "policies" , "organize" , "general" , "administration" , "needs" , "leading" , "effectively" , "ensuring" , "social" , "existing" , "dedicated" , "handle" , "related" , "problems" , "admin" , "major" , "responsibility" , "state" , "learner" , "level" , "oriented" , "challenging" , "business" , "documents" , "files" , "schedule" , "company" , "equipment" , "employees" , "processes" , "dynamic" , "solution" , "word" , "benefits" , "compensation" , "employee" , "performance" , "preparation" , "establish" , "utilize" , "achieve" , "procurement", "implementation","team","teamwork","logistics","finance","handled","manage","scheduling")
      admin_words<-unique(admin_words)
      
      data.raw <- rbind(data.raw, a7)
      data.raw<-data.raw[!apply(data.raw == "", 1, all),]
      
      library(caret)
      train<-data.raw
      library(quanteda)
      train.tokens1 <- tokens(train$resume, what = "word", 
                              remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = TRUE, remove_hyphens = TRUE)
      a<-automated_readability_index(curr_resume,rm.incomplete = TRUE)
      complexity<-a$Counts[5]/a$Counts[3]
      complexity<-round(as.numeric(complexity))
      
      train.tokens <- tokens_tolower(train.tokens1)
      train.tokens <- tokens_select(train.tokens, stopwords(), 
                                    selection = "remove")
      #train.tokens <- tokens_wordstem(train.tokens, language = "english")
      #names(train.tokens) <- make.names(names(train.tokens))
      train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
      train.tokens.matrix <- as.matrix(train.tokens.dfm)
      admin_words_cut<-c()
      for(x in 1:length(analyst_words))
      {
        if(! is.na(match(analyst_words[x],colnames(train.tokens.matrix))))
          admin_words_cut<-append(analyst_words[x],admin_words_cut)
        
      }
      train.tokens.matrix<-subset(train.tokens.matrix,select=admin_words_cut)
      
      term.frequency <- function(row) {
        row / sum(row)
      }
      
      inverse.doc.freq <- function(col) {
        corpus.size <- length(col)
        doc.count <- length(which(col > 0))
        
        log10(corpus.size / doc.count)
      }
      
      tf.idf <- function(x, idf) {
        x * idf
      }
      
      train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
      #train.tokens.df <- train.tokens.df[!apply(is.na(train.tokens.df) | train.tokens.df == "", 1, all),]
      train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
      str(train.tokens.idf)
      train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
      train.tokens.tfidf <- t(train.tokens.tfidf)
      m<-as.matrix(train.tokens.tfidf)
      m2<-t(m)
      rownames(m)<-1:nrow(m)
      norm_eucl<-function(m)
        m/apply(m,1,function(x) sum(x^2)^.5)
      
      m_norm<-norm_eucl(m)
      
      results<-kmeans(m_norm,3,10)
      rhs<-results$cluster
      weights <- relief(rhs~., as.data.frame(m_norm),neighbours.count = 3)
      weights<-abs(weights)
      cbr<-c(1:nrow(m_norm))
      for(x in 1:nrow(m_norm))
      {
        cbr[x]<-0
        for(y in 1:ncol(m_norm))
        {
          prod=weights[y,1]*m_norm[x,y]
          cbr[x]=cbr[x]+prod
        }
      }
      cbr<-cbr*10000
      
      rating_admin<-(match(cbr[nrow(m_norm)],sort(cbr))*10)/length(as.matrix(cbr))
      print(rating_admin)
      write.csv(rating_admin,file = FullPath("rating_admin.csv"))
      train.tokens.tfidf2<-train.tokens.tfidf*1000000
      
      sumidf<-apply(train.tokens.tfidf2,2,sum)
      uniqueness<-(abs(sumidf[nrow(data.raw)-1]-summary(sumidf)[4])*10)/summary(sumidf)[4]
      uniqueness<-round(as.numeric(uniqueness))
      
      
      u_c<-c(uniqueness,complexity)
      write.csv(u_c,FullPath("u_c.csv"),row.names = F)
      plot_admin<-doughnut( c(rating_admin,10-rating_admin) , labels=c("Administration","."),init.angle = 0 ,col=c("darkolivegreen2","darkolivegreen1") )
    
      return(plot_admin)
      # 
    }
  )
  output$resume_jist<-renderText(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      imp_sentences<-readtext(FullPath("resume_imp_sentences.txt"))
      return(imp_sentences)
    }
  )
  output$resume_analyst<-renderPlot(
    {
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      #Adding the new resume
      
      curr_resume<-readChar(data_resume(), file.info(data_resume())$size)
      a7<-"as"
      a7<-append(a7,curr_resume)
      data.raw<-read.csv(file=FullPath("analyst.csv"),header = T,stringsAsFactors = F)
      
      software_dev<-c("software" , "project" , "developer" , "web" , "experience" , "sql" , "end" , "php" , "developed" , "java" , "mysql" , "css" , "server" , "development" , "html" , "years" , "module" , "management" , "asp.net" , "system" , "c#" , "data" , "laravel" , "application" , "front" , "api" , "online" , "technical" , "bootstrap" , "design" , "website" , "programming" , "admin" , "database" , "windows" , "ms" , "responsibilities" , "c" , "projects" , "back" , "responsible" , "android" , "html5" , "json" , "languages" , "tools" , "systems" , "core" , "operating" , "implemented" , "master" , "role" , "rest" , "codeigniter" , "net" , "designing" , "gives" , "learn" , "thane" , "xml" , "user" , "dynamic" , "service" , "organization" , "microsoft" , "learning" , "merchant" , "professional" , "infotech" , "content" , "company" , "oracle" , "technologies" , "testing" , "done" , "business" , "engineer" , "involved" , "ui" , "creating" , "platform" , "responsive" , "apache" , "implementation" , "main" , "barcode" , "r2" , "provides" , "self" , "india" , "functionality" , "validation" , "level" , "soap" , "datapower" , "integration" , "environment" , "eclipse" , "netbeans" , "control" , "excel" , "word" , "implementing" )
      analyst_words<-c("skills" ,"modelling","analysis","analyst","analysed","preprocessing","processing","xml","ad-hoc","testing", "year" , "work" , "less" , "experience" , "analyst" , "management" , "project" , "analysis" , "financial" , "good" , "software" , "finance" , "services" , "module" , "information" , "data" , "business" , "level" , "excel" , "development" , "research" , "training" , "time" , "award" , "market" , "applications" , "analytics" , "corporate" , "sector" , "industry" , "preparing" , "organization" , "learning" , "technical" , "ncfm" , "modeling" , "positive" , "advanced" , "certification" , "office" , "learner" , "datapower" , "professional" , "engineering" , "building" , "clients" , "operations" , "key" , "results" , "model" , "developing" , "client" , "service" , "december" , "credit" , "reports" , "cfa" , "projects" , "accounting" , "database" , "report" , "consensus" , "statement" , "name" , "life" , "various" , "study" ,  "investor" , "diploma" , "markets" , "beginners" , "ability" , "crisil" , "assessing" , "grading")
      analyst_words<-unique(analyst_words)
      
      
      data.raw <- rbind(data.raw, a7)
      data.raw<-data.raw[!apply(data.raw == "", 1, all),]
      #data.raw$textlength<-nchar(data.raw$resume)
      library(caret)
      train<-data.raw
      library(quanteda)
      train.tokens1 <- tokens(train$resume, what = "word", 
                              remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = TRUE, remove_hyphens = TRUE)
      #a<-automated_readability_index(data.raw$resume[1],rm.incomplete = TRUE)
      #complexity<-a$Counts[5]/a$Counts[3]
      
      train.tokens <- tokens_tolower(train.tokens1)
      train.tokens <- tokens_select(train.tokens, stopwords(), 
                                    selection = "remove")
      #train.tokens <- tokens_wordstem(train.tokens, language = "english")
      #names(train.tokens) <- make.names(names(train.tokens))
      train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
      train.tokens.matrix <- as.matrix(train.tokens.dfm)
      analyst_words_cut<-c()
      for(x in 1:length(analyst_words))
      {
        if(! is.na(match(analyst_words[x],colnames(train.tokens.matrix))))
          analyst_words_cut<-append(analyst_words[x],analyst_words_cut)
        
      }
      train.tokens.matrix<-subset(train.tokens.matrix,select=analyst_words_cut)
      
      term.frequency <- function(row) {
        row / sum(row)
      }
      
      inverse.doc.freq <- function(col) {
        corpus.size <- length(col)
        doc.count <- length(which(col > 0))
        
        log10(corpus.size / doc.count)
      }
      
      tf.idf <- function(x, idf) {
        x * idf
      }
      
      train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
      #train.tokens.df <- train.tokens.df[!apply(is.na(train.tokens.df) | train.tokens.df == "", 1, all),]
      train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
      str(train.tokens.idf)
      train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
      train.tokens.tfidf <- t(train.tokens.tfidf)
      m<-as.matrix(train.tokens.tfidf)
      m2<-t(m)
      rownames(m)<-1:nrow(m)
      norm_eucl<-function(m)
        m/apply(m,1,function(x) sum(x^2)^.5)
      
      m_norm<-norm_eucl(m)
      #m_norm<-t(m_norm)
      results<-kmeans(m_norm,3,10)
      
      rhs<-results$cluster
      weights <- relief(rhs~., as.data.frame(m_norm),neighbours.count = 3)
      weights<-abs(weights)
      cbr<-c(1:nrow(m_norm))
      for(x in 1:nrow(m_norm))
      {
        cbr[x]<-0
        for(y in 1:ncol(m_norm))
        {
          prod=weights[y,1]*m_norm[x,y]
          cbr[x]=cbr[x]+prod
        }
      }
      cbr<-cbr*10000
      
      rating_analyst<-(match(cbr[nrow(m_norm)],sort(cbr))*10)/length(as.matrix(cbr))
      dev_value<-data.frame(read.csv(FullPath("rating_dev.csv")))
      rating_dev<-dev_value[1,2]
      admin_value<-data.frame(read.csv(FullPath("rating_admin.csv")))
      rating_admin<-admin_value[1,2]
      data_to_be_added<-data.frame(Sys.time(),name(),input$Rec_no, gender1(),rating_dev,rating_admin,rating_analyst)
      SHEET.NAME <- 'Sheet1'
      file<-FullPath("Data_resume_analysis.xlsx")
      existing.data <- read.xlsx(file, sheetName = SHEET.NAME)
      new.data <- rbind(existing.data, data_to_be_added)
      write.xlsx(new.data, file, sheetName = SHEET.NAME, row.names = F, append = F)
      plot_analyst<-doughnut( c(rating_analyst,10-rating_analyst) ,labels=c("Analysis","."),init.angle = 0, col=c("coral4","coral1") )
      return(plot_analyst)
    }
  )
  
  output$resume_uniqueness<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("u_c.csv"),header = T,stringsAsFactors = F))
    #rf_result<-subset(rf_result, select= c('uniqueness','complexity'))
    Confidence1<-as.numeric(rf_result[1,1])
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Uniqueness",
      color = "teal"
    )
  })
  output$resume_complexity<-renderValueBox({
    
    FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
    rf_result<-data.frame(read.csv(file=FullPath("u_c.csv"),header = T,stringsAsFactors = F))
    #rf_result<-subset(rf_result, select= c('uniqueness','complexity'))
    Confidence1<-rf_result[2,1]
    
    
    valueBox(
      paste0(Confidence1, "/10"), "Complexity",
      color = "aqua"
    )
  })# end of outpue resume complexity
  
  output$topTenResumes<-renderPlot(
    {
      library(ggplot2)
      attach(mtcars)
      
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      interviewee_data<-data.frame(read.xlsx(file=FullPath("Data_resume_analysis.xlsx"),sheetName = "Sheet1",header = T,stringsAsFactors = F))
      ux<-unique(interviewee_data[,2])
      n<-list()
      z=1
      sum_conf<-list()
      for(y in ux){
        #print(z)
        sum_conf[z]<-0
        n[z]<-0
        for(x in c(1:nrow(interviewee_data)))
        {
          
          if(interviewee_data[x,2]==y){
            sum_conf[z]=as.numeric(sum_conf[z])+as.numeric(interviewee_data[x,jobprof()])
            n[z]=as.numeric(n[z])+1
            
          }
          
        }
        #print("count of "+y+"="+n[z])
        sum_conf[z]<-as.numeric(as.numeric(sum_conf[z])/as.numeric(n[z]))
        
        z=z+1
      }
      
      conf<-as.numeric(sum_conf)
      
      return(barplot(conf,names.arg = ux))
      
    })#end of top ten resumes
  
  top_dev_list<-reactive(
    {
      FullPath = function(FileName){ return( paste( "D:/BE_Project/BE/resume_analysis/", FileName, sep=""))}
      if(input$jobprof=="Developer")
      {
        software_dev<-c("software" , "project" , "developer"  , "experience" , "sql" , "end"  , "developed" , "java"   , "server" , "development"  , "years" , "module" , "management"  , "system" , "c#" , "data" , "laravel" , "application" , "front"  , "online" , "technical" , "bootstrap" , "design" , "website" , "programming" , "admin" , "database" , "windows" , "ms" , "responsibilities" , "c" , "projects" , "back" , "responsible" , "android"  , "languages" , "tools" , "systems" , "core" , "operating" , "implemented" , "master" , "role" , "rest" , "codeigniter" , "net" , "designing" , "gives" , "learn" , "thane" , "user" , "dynamic" , "service" , "organization" , "microsoft" , "learning" , "merchant" , "professional" , "infotech" , "content" , "company" , "oracle" , "technologies" , "testing" , "done" , "business" , "engineer" , "involved" , "ui" , "creating" , "platform" , "responsive" , "apache" , "implementation" , "main" , "barcode" , "r2" , "provides" , "self"  , "functionality" , "validation" , "level"  , "datapower" , "integration" , "environment" , "eclipse" , "netbeans" , "control" , "excel" , "word" , "implementing" )
        web_dev<-c("äjax","api","jquery","rest","xampp","css","html5","html","css3","json","javascript","bootstrap","php","responsive","website","web","xml")
        db_dev<-c("server","db","mysql","sql","query","management","database","mongodb","rdbms","dbms","apache","tomcat")
        if("Web development" %in% input$jobspec_dev){software_dev<-unique(append(software_dev,web_dev))}
        data.raw<-read.csv(file=FullPath("software_dev.csv"),header = T,stringsAsFactors = F)
        if("Database handling and management" %in% input$jobspec_dev){software_dev<-unique(append(software_dev,db_dev))}
        #software_dev<-c("software" , "project" , "developer" , "web" , "experience" , "sql" , "end" , "php" , "developed" , "java" , "mysql" , "css" , "server" , "development" , "html" , "years" , "module" , "management" , "asp.net" , "system" , "c#" , "data" , "laravel" , "application" , "front" , "api" , "online" , "technical" , "bootstrap" , "design" , "website" , "programming" , "admin" , "database" , "windows" , "ms" , "responsibilities" , "c" , "projects" , "back" , "responsible" , "android" , "html5" , "json" , "languages" , "tools" , "systems" , "core" , "operating" , "implemented" , "master" , "role" , "rest" , "codeigniter" , "net" , "designing" , "gives" , "learn" , "thane" , "xml" , "user" , "dynamic" , "service" , "organization" , "microsoft" , "learning" , "merchant" , "professional" , "infotech" , "content" , "company" , "oracle" , "technologies" , "testing" , "done" , "business" , "engineer" , "involved" , "ui" , "creating" , "platform" , "responsive" , "apache" , "implementation" , "main" , "barcode" , "r2" , "provides" , "self" , "india" , "functionality" , "validation" , "level" , "soap" , "datapower" , "integration" , "environment" , "eclipse" , "netbeans" , "control" , "excel" , "word" , "implementing" )
        #analyst_words<-c("skills" , "year" , "work" , "less" , "experience" , "analyst" , "management" , "project" , "analysis" , "financial" , "good" , "software" , "finance" , "services" , "module" , "information" , "data" , "business" , "level" , "excel" , "development" , "research" , "training" , "time" , "award" , "market" , "applications" , "analytics" , "corporate" , "sector" , "industry" , "preparing" , "organization" , "learning" , "technical" , "ncfm" , "modeling" , "positive" , "advanced" , "certification" , "office" , "learner" , "datapower" , "professional" , "engineering" , "building" , "clients" , "operations" , "key" , "results" , "model" , "developing" , "client" , "service" , "december" , "credit" , "reports" , "cfa" , "projects" , "accounting" , "database" , "report" , "consensus" , "statement" , "name" , "life" , "various" , "study" ,  "investor" , "diploma" , "markets" , "beginners" , "ability" , "crisil" , "assessing" , "grading")
        #admin_words<-c("information" , "management" , "administrator" , "years" , "communication" , "additional" , "new" , "working" , "handling" , "services" , "learning" , "good" , "staff" , "ltd" , "knowledge" , "team" , "records" , "excel" , "help" , "maintenance" , "time" , "diploma" , "software" , "months" , "preparing" , "data" , "reports" , "prepare" , "administrative" , "computer" , "strong" , "able" , "professional" , "june" , "period" , "customer" , "calls" , "organization" , "record" , "production" , "technical" , "datapower" , "project" , "plans" , "environment" , "attitude" , "ability" , "results" , "nature" , "people" , "committed" , "manager" , "quality" , "hr" , "designation" , "current" , "systems" , "travel" , "arrangements" , "effective" , "database" , "finance" , "correspondence" , "technology" , "procedures" , "service" , "testing" , "test" , "field" , "high" , "excellent" , "personnel" , "policies" , "organize" , "general" , "administration" , "needs" , "leading" , "effectively" , "ensuring" , "social" , "existing" , "dedicated" , "handle" , "related" , "problems" , "admin" , "major" , "responsibility" , "state" , "learner" , "level" , "oriented" , "challenging" , "business" , "documents" , "files" , "schedule" , "company" , "equipment" , "employees" , "processes" , "dynamic" , "solution" , "word" , "benefits" , "compensation" , "employee" , "performance" , "preparation" , "establish" , "utilize" , "achieve" , "procurement", "implementation")
        
        data.raw <- rbind(data.raw, a7)
        data.raw<-data.raw[!apply(data.raw == "", 1, all),]
        
        library(caret)
        train<-data.raw
        library(quanteda)
        train.tokens1 <- tokens(train$resume, what = "word", 
                                remove_numbers = TRUE, remove_punct = TRUE,
                                remove_symbols = TRUE, remove_hyphens = TRUE)
        train.tokens <- tokens_tolower(train.tokens1)
        train.tokens <- tokens_select(train.tokens, stopwords(), 
                                      selection = "remove")
        #train.tokens <- tokens_wordstem(train.tokens, language = "english")
        #names(train.tokens) <- make.names(names(train.tokens))
        train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
        train.tokens.matrix <- as.matrix(train.tokens.dfm)
        software_dev2<-c()
        for(x in 1:length(software_dev))
        {
          if(! is.na(match(software_dev[x],colnames(train.tokens.matrix))))
            software_dev2<-append(software_dev[x],software_dev2)
          
        }
        train.tokens.matrix<-subset(train.tokens.matrix,select=software_dev2)
        
        term.frequency <- function(row) {
          row / sum(row)
        }
        
        inverse.doc.freq <- function(col) {
          corpus.size <- length(col)
          doc.count <- length(which(col > 0))
          
          log10(corpus.size / doc.count)
        }
        
        tf.idf <- function(x, idf) {
          x * idf
        }
        
        train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
        #train.tokens.df <- train.tokens.df[!apply(is.na(train.tokens.df) | train.tokens.df == "", 1, all),]
        train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
        str(train.tokens.idf)
        train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
        train.tokens.tfidf <- t(train.tokens.tfidf)
        m<-as.matrix(train.tokens.tfidf)
        m2<-t(m)
        rownames(m)<-1:nrow(m)
        norm_eucl<-function(m)
          m/apply(m,1,function(x) sum(x^2)^.5)
        
        m_norm<-norm_eucl(m)
        #m_norm<-t(m_norm)
        results<-kmeans(m_norm,3,10)
        
        rhs<-results$cluster
        weights <- relief(rhs~., as.data.frame(m_norm),neighbours.count = 3)
        weights<-abs(weights)
        cbr<-c(1:nrow(m_norm))
        for(x in 1:nrow(m_norm))
        {
          cbr[x]<-0
          for(y in 1:ncol(m_norm))
          {
            prod=weights[y,1]*m_norm[x,y]
            cbr[x]=cbr[x]+prod
          }
        }
        cbr<-cbr*10000
        top_dev<-tail(sort(cbr),input$num_resume)
        top_dev_list<-data.frame(fix.empty.names = F)
        for(i in c(1:input$num_resume)){
          top_dev_resume<-data.raw[match(top_dev[i],cbr),2]
          sentences<-tolower(unlist(strsplit(top_dev_resume,split="\\\n")))
          top_dev_list[i,1]<-sentences[1]
        }#end of for
        return(top_dev_list)
        
      }#end of if
      
      
    })#end of output$top_resumes
  output$ora<-renderTable(
    {
      return(top_dev_list())
    }
  )
  
}#end of server


shinyApp(ui, server)
