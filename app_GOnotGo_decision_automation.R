library(ggplot2)
library(tidyverse)
library(clinfun)
library(patchwork)
library(shinythemes)
library(DT)
library(shinyMatrix)
library(patchwork)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Go/No-Go Decisions Methods "),
  h3("Frequentist Two-Stage Methods Transitioning to Bayesian Design"),
  tabsetPanel(
    ################################ First TAB ################################
    tabPanel("Input Overview", 
             htmlOutput(outputId = "overview_input")
    ),
    ################################ Second TAB ################################ 
    tabPanel("Simon's Two Stage",
             p("This tab gives a look into a Frequentist Simon's Two Stage Approach. 
        The panel on the left controls the inputs for the following three subsection tabs.  
        The Input Overview tab gives more information about what each input means. "),
             hr(),
             column(3,
                    wellPanel( # upper panel 
                      h4("Starting Inputs for Simon's Two-Stage"),
                      numericInput(inputId = "p0", "Historical response rate (p0)", 
                                   value = 0.1, min = 0, max = 1, step = 0.05),
                      numericInput(inputId = "p1","Desirable Response Rate (p1)", 
                                   value = 0.3, min = 0.05, max = 1, step = 0.05),
                      numericInput(inputId = "a", "Type 1 error (alpha)", 
                                   value = 0.05, min = 0, max = 1, step = 0.05),
                      numericInput(inputId = "b",  "Type II error (beta)", 
                                   value = 0.1, min = 0, max = 1, step = 0.05),
                      numericInput(inputId="nmax", "Max Sample", 
                                   value=100, min = 25, max=500)
                    )
             ),
             mainPanel( # output panel 
               tabsetPanel(
                 type = "tabs",
                 # First Section Panel 
                 tabPanel("Simon's Two Stage", verbatimTextOutput(outputId = "OC1"), 
                          htmlOutput(outputId = "description"),
                          p("These outputs are obtained using the ph2simon package in the clinfun libary. "),
                 ),
                 # Second Section Panel 
                 tabPanel("Simon's Simulation", 
                          h3("Simulation Simon's Graphs"),
                          p("This page is able to compare multiple graphs with different ranges of ORR values. 
              To create more than one graph select the Add Graph button. Each graph has its own 
              tab where the range of ORR value, Historical response rate and Desirable Response Rate can 
              be updated. "),
                          wellPanel(
                            actionButton('moreL2', tags$b('Add Graph')),
                            actionButton('lessL2', tags$b('Remove Graph')),
                            uiOutput('panelset_simon') 
                          ),
                          uiOutput("dynamic_simon")
                 ),
                 
                 # Third Section Panel
                 tabPanel("Table Download Options", 
                          h3("Simulation Table"),
                          p("This data was first seen on the Simulation Simon's Graphs page."),
                          wellPanel(
                            sliderInput("slider", label = h4("True ORR value "), 
                                        min = 0, max = 1, value = c(0.05, 0.6)),
                            downloadButton("downloadData", "Download Table")
                          ),
                          tableOutput("simons_table_download")
                 )
               )
             )
    ),
    ################################ Third TAB ################################
    tabPanel("Bayesian Go/No-Go Adaptation",
             p("This tab demonstrates how we can start with a Simon's Two-Stage Design 
      and adapted it into a Bayesian Framework. This transition can make it so the 
      Dual Criteria approach is identical/comparable to the Simon's Two-Stage Design 
      when utilizing the threshold obtain when calculating the posterior distributions."),
             hr(),
             fluidRow( # output panel 
               column(11,
                      tabsetPanel(
                        type = "tabs",
                        # Fourth Section Panel 
                        tabPanel("Bayesian Go/No-Go Criteria",
                                 fluidRow(
                                   column(4,
                                          wellPanel( # upper panel 
                                            h4("Starting Inputs for Simon's Two-Stage"),
                                            numericInput(inputId = "p0_bayesian", "Historical response rate (p0)", 
                                                         value = 0.1, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "p1_bayesian","Desirable Response Rate (p1)", 
                                                         value = 0.3, min = 0.05, max = 1, step = 0.05),
                                            numericInput(inputId = "a_bayesian", "Type 1 error (alpha)", 
                                                         value = 0.05, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "b_bayesian",  "Type II error (beta)", 
                                                         value = 0.1, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId="nmax_bayesian", "Max Sample", 
                                                         value=100, min = 25, max=500)
                                          )
                                   ),
                                   column(width = 8, 
                                          htmlOutput("simons_starting"),
                                          shiny::fluidRow(
                                            shiny::column(width = 2,
                                                          h4("Stage 1 Posterior Distribution"),
                                                          DT::dataTableOutput(outputId = "stage_one_tab"),
                                                          style = "font-size: 85%; width: 50%"
                                            ),
                                            shiny::column(width = 2,
                                                          h4("Stage 2 Posterior Distribution"),
                                                          DT::dataTableOutput(outputId = "stage_two_tab"),
                                                          style = "font-size: 85%; width: 50%"
                                            )
                                          ),
                                          h3("Decision Criteria for Bayesian Design:"),
                                          htmlOutput("decision_criteria_table")
                                   )
                                 )
                        ),
                        # Firth Panel 
                        tabPanel("Bayesian Dual-Criteria Simulator",
                                 fluidRow(
                                   column(4,
                                          wellPanel(  
                                            h4("Input For Bayesian Implementation"),
                                            numericInput(inputId = "lrv", "Lower Reference Value (LRV)", value = 0.1,
                                                         min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "tv", "Target Value (TV)", value = 0.3,
                                                         min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "alpha", "alpha prior", value = 0.5, step = 0.05),
                                            numericInput(inputId = "beta","beta prior", value = 0.5, step = 0.05)
                                          )
                                   ), 
                                   column(8,
                                          h3("Simulation Dual-Criteria Graphs"),
                                          htmlOutput("bayes_decision"),
                                          h3("Threshold Specification:"),
                                          wellPanel(
                                            actionButton('moreL3', tags$b('Add Graph')),
                                            actionButton('lessL3', tags$b('Remove Graph')),
                                            uiOutput('panelset_dual')
                                          ),
                                          uiOutput("dynamic_dual")
                                   )
                                 )
                        ),
                        # Six Panel 
                        tabPanel("Table Download Options", 
                                 fluidRow(
                                   column(4,
                                          wellPanel( # upper panel 
                                            h4("Starting Inputs for Simon's Two-Stage"),
                                            numericInput(inputId = "p0_bayesian2", "Historical response rate (p0)", 
                                                         value = 0.1, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "p1_bayesian2","Desirable Response Rate (p1)", 
                                                         value = 0.3, min = 0.05, max = 1, step = 0.05),
                                            numericInput(inputId = "a_bayesian2", "Type 1 error (alpha)", 
                                                         value = 0.05, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "b_bayesian2",  "Type II error (beta)", 
                                                         value = 0.1, min = 0, max = 1, step = 0.05),
                                            numericInput(inputId="nmax_bayesian2", "Max Sample", 
                                                         value=100, min = 25, max=500)
                                          )
                                   ),
                                   column(8,
                                          h3("Stage Two Posterior Distribution Table"),
                                          p("This data was first seen on the Bayesian Criteria page in the second table."),
                                          wellPanel(
                                            downloadButton("downloadPosterior", "Download Table")
                                          ),
                                          tableOutput("posterior_table"),
                                   )
                                 )
                        ),
                        #Seven Panel
                        tabPanel("Sample Size Exploration",
                                 fluidRow(
                                   column(4,
                                          wellPanel( # lower panel
                                            h4("Input For Bayesian Implementation"),
                                            numericInput(inputId = "lrv1", "Lower Reference Value (LRV)", value = 0.1,
                                                         min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "tv1", "Target Value (TV)", value = 0.3,
                                                         min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = "alpha1", "alpha prior", value = 0.5, step = 0.05),
                                            numericInput(inputId = "beta1","beta prior", value = 0.5, step = 0.05),
                                            textInput( inputId = "sample_sizeA", label = "List of Possible Sample Sizes ", 
                                                       value = "20, 30, 35, 40, 45"),
                                            numericInput(inputId = 'thresholdA1', "Threshold1", value = 0.05,
                                                         min = 0, max = 1, step = 0.05),
                                            numericInput(inputId = 'thresholdB1', "Threshold1", value = 0.95,
                                                         min = 0, max = 1, step = 0.05),
                                            sliderInput(inputId = 'slider_orrA', label = "True ORR value ", 
                                                        min = 0, max = 1, value = c(0.05, 0.6)),
                                          )
                                   ),
                                   column(8,
                                          h3("Simulation Dual-Criteria Graphs"),
                                          htmlOutput("bayes_decision_two"),
                                          h5("Sample Size Observations:"),
                                          p("When the sample size is chosen from Simon's two phases and 
                    using the suggested thresholds the indeterminate region goes 
                    to zero. For a sample size less than the suggested one the 
                    indeterminate outcomes are most likely for the true effect sizes 
                    just above LRV. For sample size larger, the indeterminate outcomes 
                    are most likely for true effect sizes between 0 and LRV. "),
                                          plotOutput("sample_size_graphs")
                                          
                                   )
                                 )
                        ),
                        #Eight Panel
                        tabPanel("Bayesian Explanation",
                                 fluidRow(
                                   column(4, numericInput(inputId = "lrv2", "Lower Reference Value (LRV)", 
                                                          value = 0.1, min = 0, max = 1, step = 0.05) ),
                                   column(4, numericInput(inputId = "tv2", "Target Value (TV)", 
                                                          value = 0.3, min = 0, max = 1, step = 0.05)),
                                   column(4, numericInput(inputId = 'sample_sizeB',"Sample Size", value = 35, step = 1))
                                 ),
                                 
                                 h3("Prior Distribution "),
                                 p("It is advised to assume that the prior follows a minimally informative 
                unimodal beta distribution. p~beta(a=0.5, b=0.5) in the previous. 
                However, the incorporation of prior information can be made by 
                choosing different values of the shape parameters. To choose the 
                alpha and beta values previous information collected is utilized. 
                The inputs below can test multiple different parameters for the prior distribution."),
                                 column(6, textInput( inputId = "alpha2", label = "List of alpha prior values ", 
                                                      value = "0.5, 1, 2, 0.5, 1, 10") ),
                                 column(6, textInput( inputId = "beta2", label = "List of beta prior values ", 
                                                      value = "0.5, 1, 2, 1, 0.5, 1") ),
                                 plotOutput("prior_plot"),
                                 p(" ."),
                                 p(". "),
                                 hr(),
                                 uiOutput('math_prior'),
                                 p("Using these two equations and the previous information from past studies 
                the prior can be updated to include relevant information. Therefore, 
                if in previous studies the expected treatment effect is collected the 
                a and b parameters can be calculated. This produces multiple pairs of 
                a and b values.  To choose the optimal pair the previous study information 
                about the variance or standard deviation can be used by finding the 
                pair of a and b values that have a matching variance. "),
                                 
                                 hr(),
                                 h3("Likelihood Distribution "),
                                 p("The next piece of the bayesian puzzle is the likelihood function.
                Before, jumping straight into the likelihood the first graph displays 
                a series of pmf functions based on the Binomial distribution. 
                This is equivalent of saying if the true ORR rate were given 
                some value of p, then how many of the n
                participants would show a positive result from the treatment? 
                The plot below summarizes a range of p values. 
                The highlighted lines denote the number of patients if LRV was true and if
                the TV was true. 
                "),
                                 plotOutput("likelihood_part1"),
                                 p("From all the pmf plots we see that when the possible value of p is 
              low then the outcome results are also expected to be below. The opposite 
              happens when the possible p-value is high in the outcome result is also 
              expected to be high.  In reality, let's say we observe either LRV or the 
              TV percentage of n participants to have a positive response. To focus on 
              just these results we can extract the highlighted lines from above and 
              put them on one single plot shown below. Thus constructing our likelihood 
              function for any p between 0 and 1. Here the likelihood is a function 
              of p that can help provide insight into the relative compatibility of 
              the observed patient outcome data for LRV and TV.
                "),
                                 plotOutput("likelihood_part2"),
                                 
                                 hr(),
                                 h3("Posterior Distribution "),
                                 p("Now that we have explored both the prior and likelihood functions 
                we have the two pieces to construct our posterior distribution. 
                Both functions are used in the construction and below shows all the 
                distributions together on a graph.  To explore how the prior impacts 
                the posterior used the inputs to update the prior information parameters 
                to see the effect."),
                                 column(6, numericInput(inputId = "alpha3", "alpha prior", value = 10, step = 0.05)),
                                 column(6, numericInput(inputId = "beta3","beta prior", value = 10, step = 0.05) ),
                                 plotOutput("posterior_part2"),
                                 p("."),
                                 p("."),
                                 p("."),
                                 p("We can now use all of the information to construct the posterior 
                distributions related to the decision-making process of GO/NO-GO. 
                The below plot shows the posterior probabilities of both the LRV 
                and TV functions which are used to chose the thresholds in Simon's 
                transition. For Simon's r and r1 value, the corresponding probability 
                is given on the y-axis which becomes the go/no-go threshold. Below 
                shows how the prior information influences the choice of thresholds. 
                Update the prior values to see how it changes the threshold probabilities"),
                                 column(6, textInput( inputId = "alpha4", label = "List of alpha prior values ", 
                                                      value = "0.5, 1, 2, 0.5, 1,10") ),
                                 column(6, textInput( inputId = "beta4", label = "List of beta prior values ", 
                                                      value = "0.5, 1, 2, 1, 0.5,1") ),
                                 plotOutput("posterior_part1"),
                                 p("."),
                                 p("."),
                                 p("."),
                                 p("When bigger a and b values are given, the posterior threshold 
                calculations are more affected. When small values of a and b are 
                given then the posterior distributions all converge to the same 
                probabilities with minimal variations. ")
                        )
                      )
               )
             )
    ),
    ################################ Fourth TAB ################################
    tabPanel("Fleming's Two Stage",
             p("This tab gives a look into a Frequentist Fleming's Two Stage Approach. 
        The Input Overview tab gives more information about what each input means. 
        This method is here for completeness, it is comparable to the Simon's Two-Stage
        tab. This method differs from the Simon's Two Stage because it has both 
        the option for stopping for efficacy as well as futility. The Simon's 
        two stage just stops the trial based on futility. Since we have two 
        ways to stop the trial, the design output reports two values for 
        each stage to help determine the go/no-go rulings."),
             hr(),
             column(4,
                    wellPanel( # panel for inputs 
                      h4("Starting Inputs for Fleming's Two-Stage"),
                      numericInput(inputId = "p0_fleming", "Historical response rate (p0)",
                                   value = 0.3, min = 0, max = 1, step = 0.05),
                      numericInput(inputId = "pa_fleming","Desirable Response Rate (pa)",
                                   value = 0.5, min = 0.05, max = 1, step = 0.05),
                      numericInput(inputId = "alpha_fleming", "Type 1 error (alpha)",
                                   value = 0.05, min = 0, max = 1, step = 0.05),
                      numericInput(inputId = "tries",  "Number of Sample Size Scenarios",
                                   value = 4, min = 1, max = 10, step = 1),
                      uiOutput('panelset_fleming')
                    )
             ),
             mainPanel( # output panel 
               DT::dataTableOutput(outputId = "fleming"),
               h4("Write up for a protocal suggestion:"),
               htmlOutput("fleming_description")
             )
    )
  )
)


server <- function(input, output) {
  
  source("functions_needed.R")
  source("fleming.R")
  
  ################################################################################################
  #######################                  Tab 1,  Overview                #######################
  ################################################################################################
  output$overview_input <- renderText({
    c("<h3>Purpose: </h3>
    
    The goal of this app is to show how starting with Simon's Two-Phase Design 
    can be adapted to a dual criterion bayesian design. The progression of tabs 
    starts with a traditional Simon's Two-Stage design, then using Simon's 
    Two-Stage output as a starting point the user can see the transition to a 
    Bayesian framework by viewing the posterior and corresponding cutoff threshold 
    distribution in tabular format. Furthermore, the following tabs show how simulations 
    of different true OR result in different go/ no-go decisions. For completeness, 
    Fleming's two-stage was also included. This tab can be compared to Simon's two-stage 
    results. To read more about Simon's Two-Phase Design, the Dual Criterion Design, 
    and the Fleming's design please select the corresponding link:
    <ul>
      <li>Simon's Two-Stage: <a href='https://www.sciencedirect.com/science/article/pii/0197245689900159'> 
      Simon, R. (1989). Optimal two-stage designs for phase II clinical trials. 
      Controlled clinical trials, 10(1), 1-10.</a> </li>
      <li>Dual Criterion: <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1002/pst.1746'>
      Frewer, P., Mitchell, P., Watkins, C., & Matcham, J. (2016). Decision‐making 
      in early clinical drug development. Pharmaceutical statistics, 15(3), 255-263.</a> </li>
      <li>Fleming's Two-Stage: <a href ='https://www.jstor.org/stable/pdf/2530297.pdf'> 
      Fleming, T. R. (1982). One-sample multiple testing procedure for phase II 
      clinical trials. Biometrics, 143-151.</a></li>
      <li>Average Sample Size Calculations: <a href ='https://www.jstor.org/stable/pdf/2529393.pdf'> 
      Schultz, J. R., Nichol, F. R., Elfring, G. L., & Weed, S. D. (1973). Multiple-stage 
      procedures for drug screening. Biometrics, 293-300.</a> </li>
    </ul>
    <h3>To learn more about what each tab see below: </h3>
    
    <h3>Tab: Simon's Two Stage</h3>
      <ol>
        <li><h4>Section: <b>Simon's Two Stage</b></h4></li>
        The user needs to specify:
        <ul>
          <li><b>Historical Response Rate</b> - This value is usually specified by a 
            clinician and takes on a value between 0 and 1. It should be less than the 
            desired response rate. </li>
          <li><b>Desirable Response Rate</b>- This value is also usually specified by a 
            clinician and takes on a value between 0 and 1. </li>
          <li> <b>Type I Error</b> - Also known as alpha this value represents the 
            number of false positives or the error of rejecting a null hypothesis when 
            it is actually true. Values should be between 0 and 1. </li>
          <li> <b>Type II Error</b> - Also known as beta this value represents the 
            number of false negatives or the error of not rejecting a null hypothesis 
            when the alternative hypothesis is the true state of nature. Power = 1 - beta. 
            Values should be between 0 and 1.</li> 
        </ul> 
        This first tab has an underlying frequentist approach. But the <b>goal</b> of 
        this app is to transition this frequentist approach to a Bayesian approach. The 
        next tabs will explore this transition. 
        </br> 
        </br> 
       
        <li><h4>Section: <b>Simon's Simulation</b></h4></li>
          The <b>Type I & II</b> errors from the top entry box do influence the input but 
          the user has the option to make up to 4 graphs with various ORR ranges and null 
          value and desired response rate. By clicking the  <b> Add Graph</b> button the 
          user has the ability to add more graphs to compare a bunch of values. 
         
        </br> 
        </br> 
         
        <li><h4>Section: <b>Table Download Options</b></h4></li>
          The tables on this page are results displayed graphically on other pages.  
          The user can download the table for their own use using the download buttons.
          The table is the Simulation Results from the Simulations Simon's Graphs tab. 
      </ol>
     
    <h3> Tab: Bayesian Go/No-Go Adaptation </h3>
      <ol>
        <li><h4>Section: <b>Bayesian Criteria</b></h4> </li>
          The user needs to specify the same information as Simon's Criteria but now it
          will transition to a Bayesian framework. The initial p0 and p1 will transition 
          to be the lower reference value (LRV) and the target value (TV). The prior 
          information usually stays Beta(alpha=0.5, beta=0.5) which specifies an uninformative 
          prior but in this app, it can be updated. The page explains more on how the 
          posterior distributions are calculated and the probably cutoff values are calculated. 
         
        </br> 
        </br> 
        <li><h4>Section: <b> Simulation Dual Criteria Graphs</b></h4></li> 
          The goal of this page is to show how using the thresholds obtain on the Bayesian 
          Criterion tab does make the dual criterion approach approximate Simon's Two-Stage Method. 
          Simon's Two-Stage design does not have any gray area because it has either 
          go/ no-go decisions, therefore using the thresholds obtained from the Bayesian 
          Criterion results in the gray line going to zero. However,  
          when we change the threshold cutoff values then the gray area appears. 
          The user needs to specify the same parameters from above as well as a few more:
          <ul>
            <li><b>Lower Reference Value (LRV)</b> - This value is distinct to the 
              Historical Response Rate but also should only take on values between 0 and 1. </li>
            <li><b>Target Value (TV)</b> - This value should be larger than the LRV but is bounded by 1. </li>
            <li><b>Prior-a</b>- This is the first shape parameter on the beta prior distribution</li> 
            <li><b>Prior-b</b>- This is the second shape parameter on the beta prior distribution</li>
          </ul>
          As well as, the inputs on the two left panels as well as the <b>Thresholds 1 & 2</b> 
          inputs in the top middle panel control the graph. 
         
        </br> 
        </br> 
        <li><h4>Section: <b>Table Download Options</b> </h4> </li>
          The tables on this page are results displayed graphically on other pages.  
          The user is able to download the table for their own use using the download buttons.
          The table is the posterior distribution of the Stage Two results from the Bayesian Criteria tab. 
         
        </br> 
        </br> 
        <li><h4>Section: <b> Sample Size Exploration</b> </h4> </li>
          This tab explores how different sample sizes affect the decision boundaries and gray region. 
          In selecting the sample size there are two criteria to think about. One is to reduce 
          the gray region. The other is to select based on the TV boundary region. Go to 
          this tab explores how each criteria changes are the sample size increases or decreases. 
         
        </br> 
        </br> 
        <li><h4>Section: <b> Bayesian Explanation</b> </h4> </li>
          This tab goes through an explanation of the bayesian setup. It explains what 
          the prior distribution is and how to select the parameters. It goes through 
          the likelihood function and how both the prior and likelihood construct the 
          posterior distribution.  This is all then related to how Simon's Two-Stage is 
          transitioned to the dual criteria through the selection of thresholds. 
         
       </ol>
     
     
    <h3>Tab: Fleming's Two Stage</h3>
      <ol>
        <li><h4>Section: <b>Fleming's Two Stage</b></h4></li>
        Fleming's method is another method that is comparable to Simon's Two-Stage. 
        However, this method allows for early termination for negative toxicity 
        response as well positive results. The user input required:
        <ul>
          <li><b>p0:</b> The largest response proportion which, if true, implies 
          that the treatment does not warrant further study. Also known as the response 
          rate of poor treatment. </li>
          <li><b>p1:</b> The smallest response proportion which, if true, implies that the 
          treatment does warrant further study. Also known as the response 
          rate of a good treatment. </li>
          <li><b>Type I Error: (alpha):</b> This value is typically 5%. </li>
          <li><b>Number of Sample Size Scenarios:</b> This value is how many sample 
          size combinations to compare.</li>
          <li><b>Each Scenario's Sample Size: </b>This matrix hold all the sample size scenarios
          Each scenario should have two numbers that represent each stage's number of patients.</li>
        </ul>
        The outputed results show the total sample size, corrected desired response rate (pa),
        the acceptance points for each stage (a1, a2), the rejection point for each state (r1, r2),
        the corrected Type I and II errors, and the average sample number for both historical response (p1)
        rate and the desired response rate (pa). 
      
      </ol>
      ")
  })
  
  ##################################################################################################
  #######################        Tab 2, Section 1: Simon's Two Stage         #######################
  ##################################################################################################
  output$OC1 <-  renderPrint({  
    # user input 
    p0   = input$p0
    p1   = input$p1
    a    = input$a
    b    = input$b
    nmax = input$nmax
    
    ph2simon(p0, p1, a, b, nmax = nmax)  # output 
  })
  
  output$description <- renderText({
    p0 = input$p0
    p1 = input$p1
    a = input$a
    b = input$b
    nmax = input$nmax
    
    # simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    minimax = simon_results$out[which(simon_results$out[,4] ==min(simon_results$out[,4])),]
    
    c("For the <b>Simon's Two Stage design</b> the algorithm has two options for chosen parameters, 
      either the <b>Optimal Design</b> or the <b>Minimax Design</b>. <br>
      <ul>
        <li>The <b>Optimal Design</b> selects the parameters 
      that produce the smallest expected sample size (EN).</li>
        <li>The <b>Minimax Design</b> selects the parameters 
      that produce the smallest total sample size (n). </li>
      </ul> 
      The model also gives the expected sample size (<i>EN</i>) and the probability of
      early termination (<i>PET</i>).
      </br>
      </br>
      <b> Protocol Template using Optimal design</b>
      <ul>
        <li>The null hypothesis that the true response rate is ",p0 ,"will be tested 
        against a one-sided alternative. In the first stage, ", optimal[[2]]," 
        patients will be accrued. If there are ", optimal[[1]]," or fewer responses in these 
        ", optimal[[2]]," patients, the study will be stopped. Otherwise, ", 
      optimal[[4]]-  optimal[[2]]," additional patients will be accrued for a total 
        of ", optimal[[4]],". The null hypothesis will be rejected if ", optimal[[3]]+1," or 
        more responses are observed in ", optimal[[4]]," patients. This design yields 
        a type I error rate of ", a," and power of ", 1-b," when the true response rate 
        is ", p1,".</li>
      </ul>
      
      <b> Protocol Template using Minimax design</b>
      <ul>
        <li>The null hypothesis that the true response rate is ",p0 ,"will be tested 
        against a one-sided alternative. In the first stage, ", minimax[[2]]," patients 
        will be accrued. If there are ", minimax[[1]]," or fewer responses in these 
        ", minimax[[2]]," patients, the study will be stopped. Otherwise, ", 
      minimax[[4]]-  minimax[[2]]," additional patients will be accrued for a total 
        of ", minimax[[4]],". The null hypothesis will be rejected if ", minimax[[3]]+1," or 
        more responses are observed in ", minimax[[4]]," patients. This design 
        yields a type I error rate of ", a," and power of ", 1-b," when the true response 
        rate is ", p1,".</li>
      </ul>
      
      <b>To update the output please change:</b>
      <ul>
        <li>Historical Response Rate  </li>
        <li>Desirable Response Rate  </li>
        <li>Type I Error </li>
        <li>Type II Error</li> 
        <li>Maximum Sample Size</li>
      </ul> 
      
      ")
  })
  
  ##################################################################################################
  #######################       Tab 2, Section 2: Mulitple ORR graphs        #######################
  ##################################################################################################
  output$panelset_simon <- renderUI({
    n <- seq(max(input$moreL2 - input$lessL2 + 1, 1))
    tabList <- lapply(n, function(i) {
      tabPanel(
        title = paste0('Graph', i),
        numericInput(inputId = paste0('p0', i), "Historical response rate", 
                     value = 0.1, min = 0, max = 1, step = 0.05),
        numericInput(inputId = paste0('p1', i), "Desirable Response Rate", 
                     value = 0.3, min = 0.05, max = 1, step = 0.05),
        sliderInput(inputId = paste0('slider', i), label = "True ORR value ", 
                    min = 0, max = 1, value = c(0.05, 0.6)),
        downloadButton(outputId = paste0('Download_Graph', i), 
                       label = paste0('Download Graph', i))
      )
    })
    do.call(tabsetPanel, tabList)
  })
  
  output$dynamic_simon <- renderUI({
    n <- seq(max(input$moreL2 - input$lessL2 + 1, 1))
    lapply(n, function(i) {
      list(
        h5(paste0("Graph", i)),
        plotOutput(paste0('L2plot', i)),
        br()
      )
    })
  })
  
  plot_simon_sims <- function(p0, p1, rate1, rate2, a, b, nmax) {
    # simon's two stage 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    # Running simulations 
    rate   = seq(rate1, rate2, 0.05) #using user input to get the rate range 
    result = go_nogo_decisions(n=optimal[[2]], N=optimal[[4]],# function from above 
                               lower = optimal[[1]]+1, upper = optimal[[3]]+1, rate)
    colnames(result) = c("rate", "pet", "P(GO)", "P(NO-GO)")
    
    table_df = as_tibble(result) %>%
      rename(True.ORR = rate,
             GO = `P(GO)`,
             NOGO = `P(NO-GO)`)
    
    # the graph is the output 
    plot_results <- table_df %>%
      pivot_longer(
        c(GO,NOGO),
        names_to = "Decision",
        values_to = "probs"
      ) %>%
      ggplot(aes(x = True.ORR, y = probs, color = Decision))+
      geom_line()+
      geom_point() +
      geom_vline(xintercept = p0) + 
      geom_vline(xintercept = p1) + 
      labs(
        x = "True ORR",
        y = "Probabilitiy",
        title = "Decision frame work Simulations",
        subtitle = str_c("p0= ",p0,", p1= ",p1 )
      ) + 
      scale_color_manual(values = c("#00AFBB", "red", "#E7B800")) +
      theme_bw()
    return(plot_results)
  }
  
  for (i in 1:TMAX) {
    local({
      my_i = i
      
      L2plot = paste0('L2plot', my_i) # Outputs
      
      list( 
        output[[L2plot]] <- renderPlot({
          #user input 
          p0    = input[[paste0('p0', my_i)]]
          p1    = input[[paste0('p1', my_i)]]
          rate1 = input[[paste0('slider', my_i)]][1]
          rate2 = input[[paste0('slider', my_i)]][2]
          a     = input$a
          b     = input$b
          nmax  = input$nmax
          
          plot_simon_sims(p0, p1, rate1, rate2, a, b, nmax)
          
        })
      )
    })
  }
  
  # Saving the graphs
  for (k in 1:TMAX) {
    local({
      my_k = k
      L2download = paste0('Download_Graph', k) # Outputs
      
      list( 
        output[[L2download]] <- downloadHandler(
          filename = paste0('Simon_Simulation_Graph', my_k),
          content = function(file) {
            #user input 
            p0    = input[[paste0('p0', my_k)]]
            p1    = input[[paste0('p1', my_k)]]
            rate1 = input[[paste0('slider', my_k)]][1]
            rate2 = input[[paste0('slider', my_k)]][2]
            a     = input$a
            b     = input$b
            nmax  = input$nmax
            
            device <- function(..., width, height) {
              grDevices::png(..., width = width, height = height,
                             res = 300, units = "in")
            }
            ggsave(file, plot =  plot_simon_sims(p0, p1, rate1, rate2, a, b, nmax), 
                   device = device )
          }) 
      )
    })
  }
  
  ##################################################################################################
  #######################   Tab 2, Section 3: Simulation Table Download      #######################
  ##################################################################################################
  tab_sim <- reactive({
    #user input 
    p0      = input$p0
    p1      = input$p1
    a       = input$a
    b       = input$b
    nmax    = input$nmax
    rate1   = input$slider[1]
    rate2   = input$slider[2]
    
    # simon's two stage 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    # Running simulations 
    rate   = seq(rate1, rate2, 0.05) #using user input to get the rate range 
    result = go_nogo_decisions(n=optimal[[2]], N=optimal[[4]],# function from above 
                               lower = optimal[[1]]+1, upper = optimal[[3]]+1, rate)
    colnames(result) = c("rate", "pet", "P(GO)", "P(NO-GO)")
    table_df = as_tibble(result) %>%
      rename(True.ORR = rate,
             GO = `P(GO)`,
             NOGO = `P(NO-GO)`)
    
    # table of the results. this is the output 
    table_df %>%
      mutate(
        GO = round(GO,2),
        NOGO = round(NOGO,2)
      ) 
  })
  
  output$simons_table_download <- renderTable({
    tab_sim()
  })
  
  output$downloadData <- downloadHandler( # Downloadable csv of selected dataset ---- 
                                          filename = function() {
                                            paste("sim_table", ".csv", sep = "")
                                          },
                                          content = function(file) {
                                            write.csv(tab_sim(), file, row.names = FALSE)
                                          }
  )
  
  ##################################################################################################
  #######################         Tab 3, Section 4: Bayesian Criteria        #######################
  ##################################################################################################
  output$stage_one_tab <- DT::renderDataTable({ # Stage one posterior table 
    # user imput 
    TV      = input$tv
    LRV     = input$lrv
    prior_a = input$alpha
    prior_b = input$beta
    p0      = input$p0_bayesian
    p1      = input$p1_bayesian
    a       = input$a_bayesian
    b       = input$b_bayesian
    nmax    = input$nmax_bayesian
    
    # Simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] == min(simon_results$out[,5])),]
    
    # first stage 
    prob1 = posterior(n=optimal[[2]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Decision = ifelse(n <= optimal[[1]], "No-Go", "Go")
      ) %>% 
      rename(
        TV_Probs = TVvec,
        LRV_Probs = LRVvec,
        Patients = n 
      )  %>%
      mutate(
        TV_Probs = round(TV_Probs, 4),
        LRV_Probs = round(LRV_Probs, 4)
      )
    
    options(DT.options = list(pageLength = 15))
    
    return(datatable(prob1, rownames = FALSE) %>% 
             DT::formatStyle(
               'Decision',  
               target = 'row',
               backgroundColor = styleEqual(c("No-Go", "Go"), c('indianred', 'skyblue'))
             )
    ) 
    
  })
  
  
  output$stage_two_tab <- DT::renderDataTable( { # Stage two posterior table 
    # user imput 
    TV      = input$tv
    LRV     = input$lrv
    prior_a = input$alpha
    prior_b = input$beta
    p0      = input$p0_bayesian
    p1      = input$p1_bayesian
    a       = input$a_bayesian
    b       = input$b_bayesian
    nmax    = input$nmax_bayesian
    
    # Simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] == min(simon_results$out[,5])),]
    
    # Stage two 
    prob2 = posterior(n=optimal[[4]], LRV, TV, prior_a, prior_b ) %>%
      mutate(
        Decision = ifelse(n<=optimal[[3]], "No-Go", "Go")
      ) %>% 
      rename(
        TV_Probs = TVvec,
        LRV_Probs = LRVvec,
        Patients = n 
      ) %>%
      mutate(
        TV_Probs = round(TV_Probs, 4),
        LRV_Probs = round(LRV_Probs, 4)
      )
    
    options(DT.options = list(pageLength = 15))
    
    return(
      datatable(prob2, rownames = FALSE) %>% DT::formatStyle(
        'Decision',  
        target = 'row',
        backgroundColor = styleEqual(c("No-Go", "Go"), c('indianred', 'skyblue')))
    ) 
  })
  
  output$simons_starting <- renderText({
    # user input 
    p0      = input$p0_bayesian
    p1      = input$p1_bayesian
    a       = input$a_bayesian
    b       = input$b_bayesian
    nmax    = input$nmax_bayesian
    TV = p0
    LRV = p1
    prior_a = 0.5
    prior_b = 0.5
    
    # simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    # decision boundary 
    paste("<b>From the pervious Section the Simon's Two Stage Results were n1 = ", 
          optimal[[2]],", r1 = ", optimal[[1]],",
      an n = ",optimal[[4]], " with a corresponding r =   ",optimal[[3]] ,". </b> </br> 
      <ul>
        <li>This implies that for the <b>first stage</b> out of  ",optimal[[2]], " 
            participants, the decision is to not go if the number of 
            responders is less than or equal to ",optimal[[1]] ,", and the decision 
            is to go if the number of responders is more than or equal to ",optimal[[1]]+1 ,".
        </li>
        <li>For the <b>Second stage</b> out of  ",optimal[[4]], " participants, the decision 
            is to not go if the number of 
            responders is less than or equal to ",optimal[[3]] ,", and the decision 
            is to go if the number of responders is more than or equal to ",optimal[[3]]+1 ,".
        </li> 
      </ul>
      Transitioning Simon's Two-phase design into a Bayesian framework we need:
      <ul>
        <li> Find the Corresponding Lower Reference Value (LRV) and Target Value (TV). </li>
        <ul> 
          <li> The LRV and TV usually match the Historical response rate (p0) 
          and Desirable Response Rate (p1).</li>
        </ul>
        <li> Find the correct corresponding posterior distribution. </li>
        <ul> 
          <li> The likelihood of the data follows a binomial distribution, the prior 
              is a minimally informative unimodal beta distribution. 
              Therefore, the initial prior values will be Beta(alpha=0.5, beta=0.5). 
              Therefore, we can approximate the posterior distribution using a 
              beta distribution where the parameters are obtained from
              Simon's Two-Stage output.
          </li>
        </ul>
      </ul>
      Therefore, the LRV = ", p0," and the TV = ", p1,", alpha = 0.5, and beta =0.5. 
      <h5>Below are the posterior distributions for the first and second stages of the trial design.</h5> "
    )
  })
  
  output$decision_criteria_table <- renderText({
    # user input 
    p0      = input$p0_bayesian
    p1      = input$p1_bayesian
    a       = input$a_bayesian
    b       = input$b_bayesian
    nmax    = input$nmax_bayesian
    TV      = p0
    LRV     = p1
    prior_a = 0.5
    prior_b = 0.5
    
    # simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] == min(simon_results$out[,5])),]
    
    # stage one 
    prob1 = posterior(n=optimal[[2]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[1]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>%
      filter(n == optimal[[1]]+1)
    
    # stage two 
    prob2 = posterior(n=optimal[[4]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[3]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>%
      filter(n == optimal[[3]]+1)
    
    # results 
    first = prob1 %>% filter(posterior_probs == "TVvec")  %>% pull(values) %>% round(4)
    two   = prob1 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    three = prob2 %>% filter(posterior_probs == "TVvec")  %>% pull(values )%>% round(4)
    four  = prob2 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    
    # decision boundary 
    paste("<h4><b>For Stage 1 Go/No-Go</b>: Go if P (ORR >",TV ,") ≥", first, " and 
      P (ORR >",LRV ,") ≥ ", two, ".<br> <b>For Stage 2 Go/No-Go</b>: Go if 
      P (ORR >",TV ,") ≥", three, " and P (ORR >", LRV,") ≥ ", four, ". </h4>Therefore 
      for a sample size of ",optimal[[4]], " the decision is to not go if the number of 
      responders is less than or equal to ",optimal[[3]] ,", and the decision is to 
      go if the number of responders is more than or equal to ",optimal[[3]]+1 ,".
      <br> 
      <br>
      For this method, the user needs to specify additional information on top of 
      the info used to run the Simon's two stage design. 
    
      The prior information usually stays Beta(a=0.5, b=0.5) which specifies an 
      uninformative prior but in this app it can be updated."
    )
    
  })
  
  
  ##################################################################################################
  #######################         Tab 3, Section 5: Dual Criteria            #######################
  ##################################################################################################
  output$panelset_dual <- renderUI({
    n <- seq(max(input$moreL3 - input$lessL3 + 1, 1))
    tabList1 <- lapply(n, function(j) {
      tabPanel(
        title = paste0('Graph', j),
        numericInput(inputId = paste0('sample_size', j),"Sample Size", value = 35, step = 1),
        numericInput(inputId = paste0('threshold1', j), "Threshold1", value = 0.05,
                     min = 0, max = 1, step = 0.05),
        numericInput(inputId = paste0('threshold2', j), "Threshold2", value = 0.95,
                     min = 0, max = 1, step = 0.05),
        sliderInput(inputId = paste0('slider_orr', j), label = "True ORR value ", 
                    min = 0, max = 1, value = c(0.05, 0.6)),
        downloadButton(outputId = paste0('Download_Graph_Dual', j), 
                       label = paste0('Download Graph', j))
      )
      
    })
    do.call(tabsetPanel, tabList1)
    
  })
  
  output$dynamic_dual <- renderUI({
    n1 =  seq(max(input$moreL3 - input$lessL3 + 1, 1))
    lapply(n1, function(j) {
      list(
        h5(paste0("Graph", j)),
        plotOutput(paste0('L3plot', j)),
        br()
      )
    })
  })
  
  plot_dual_approx <- function(tv, lrv, a, b, threshold1, threshold2, rate1, rate2, n) {
    orr   = seq(rate1, rate2, 0.05)
    
    out_graph <- decision_table(n, threshold1, threshold2, orr, lrv, tv, a, b) %>%
      pivot_longer(
        c('P(GO)', 'P(NO-GO)', 'P(INDT)'),
        names_to = "decision",
        values_to = "Probability"
      ) %>%
      ggplot(aes(x=ORR, y=Probability, color = decision)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = lrv) + 
      geom_vline(xintercept = tv) + 
      scale_color_manual(values = c("#00AFBB", "gray", "red"))+
      labs(
        title = str_c("Dual-Criteria Plot for N=",n),
        subtitle =  str_c(" LRV = ", lrv, ", TV = ", tv, ", Threshold1 =",
                          threshold1, ", Threshold2 =", threshold2)
      )+
      theme_bw()
    return(out_graph)
  }
  
  for (j in 1:TMAX2) {
    
    local({
      my_j <- j
      
      L3plot = paste0('L3plot', my_j) # Outputs
      
      list(
        output[[L3plot]] <- renderPlot({
          #user input
          tv    = input$tv
          lrv   = input$lrv
          a     = input$alpha
          b     = input$beta
          
          threshold1 = input[[paste0('threshold1', my_j)]]
          threshold2 = input[[paste0('threshold2', my_j)]]
          rate1      = input[[paste0('slider_orr', my_j)]][1]
          rate2      = input[[paste0('slider_orr', my_j)]][2]
          n          = input[[paste0('sample_size', my_j)]] 
          
          plot_dual_approx(tv, lrv, a, b, threshold1, threshold2, rate1, rate2, n)
          
        })
      )
    })
  }
  
  # Saving the graphs
  for (m in 1:TMAX2) {
    local({
      my_m = m
      dual_download = paste0('Download_Graph_Dual', m) # Outputs
      
      list( 
        output[[dual_download]] <- downloadHandler(
          filename = paste0('Dual_Criteria_Graph', my_m),
          content = function(file) {
            #user input 
            tv    = input$tv
            lrv   = input$lrv
            a     = input$alpha
            b     = input$beta
            
            threshold1 = input[[paste0('threshold1', my_m)]]
            threshold2 = input[[paste0('threshold2', my_m)]]
            rate1      = input[[paste0('slider_orr', my_m)]][1]
            rate2      = input[[paste0('slider_orr', my_m)]][2]
            n          = input[[paste0('sample_size', my_m)]] 
            
            device <- function(..., width, height) {
              grDevices::png(..., width = width, height = height,
                             res = 300, units = "in")
            }
            ggsave(file, plot =  plot_dual_approx(tv, lrv, a, b, threshold1, threshold2, rate1, rate2, n), 
                   device = device )
          }) 
      )
    })
  }
  
  output$bayes_decision <- renderText({
    # user input 
    TV      = input$tv
    LRV     = input$lrv
    prior_a = input$alpha
    prior_b = input$beta
    p0      = input$p0_bayesian
    p1      = input$p1_bayesian
    a       = input$a_bayesian
    b       = input$b_bayesian
    nmax    = input$nmax_bayesian
    
    # simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    # stage one 
    prob1 = posterior(n=optimal[[2]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[1]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>%
      filter(n == optimal[[1]]+1)
    
    # stage two 
    prob2 = posterior(n=optimal[[4]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[3]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>% 
      filter(n == optimal[[3]]+1)
    
    # results 
    first = prob1 %>% filter(posterior_probs == "TVvec")  %>% pull(values) %>% round(4)
    two   = prob1 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    three = prob2 %>% filter(posterior_probs == "TVvec")  %>% pull(values )%>% round(4)
    four  = prob2 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    
    # decision boundary 
    paste("From the third tab named: <b> Bayesian Criteria</b> The decision boundaries 
      for the Simon's Two Stage converted to a bayesian design are: 
      <br>
      <h5> <b>For Stage 2 Go/No-Go</b>: Go if P (ORR >",TV ,") ≥", three, "= threshold1, 
      and P (ORR >", LRV,") ≥ ", four,"= threshold2. The suggested sample size 
      is ", optimal[[4]] ,". </h5> If you use values from stage 2 as the sample 
      size and thresholds then the graph should approximate the Simon's two stage design. 
      This is seen by the indecisive gray line going to zero. This is because in 
      the Simon's Two Stage Design there is no indecisive region. However, when 
      you update the thresholds to differ then the indecisive region will  appear. " 
    )
    
  })
  
  ##################################################################################################
  #######################       Tab 3, Section 6: Simulation Table           #######################
  ##################################################################################################
  tab_bayesian <- reactive({
    # user imput 
    p0      = input$p0_bayesian2
    p1      = input$p1_bayesian2
    a       = input$a_bayesian2
    b       = input$b_bayesian2
    nmax    = input$nmax_bayesian2
    TV = p0
    LRV = p1
    prior_a = 0.5
    prior_b = 0.5
    
    # Simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b, nmax = nmax) 
    optimal = simon_results$out[which(simon_results$out[,5] == min(simon_results$out[,5])),]
    
    # Stage two 
    posterior(n=optimal[[4]], LRV, TV, prior_a, prior_b ) %>%
      mutate(
        Desision = ifelse(n<=optimal[[3]], "No-Go", "Go")
      ) %>% 
      rename(
        TV_Posterior = TVvec,
        LRV_Posterior = LRVvec,
        Patient_Number = n 
      )
  })
  
  output$posterior_table <- renderTable({
    tab_bayesian()
  })
  
  output$downloadPosterior <- downloadHandler(
    filename = function() {
      paste("stage_two_posterior_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tab_bayesian(), file, row.names = FALSE)
    }
  )
  
  ###########################################################################################
  ##################      Tab 3, Section 7: Sample Size Exploration        ##################
  ###########################################################################################
  
  output$sample_size_graphs <- renderPlot({
    tv    = input$tv1
    lrv   = input$lrv1
    a     = input$alpha1
    b     = input$beta1
    
    threshold1 = input$thresholdA1
    threshold2 = input$thresholdB1
    rate1      = input$slider_orrA[1]
    rate2      = input$slider_orrA[2]
    n          = input$sample_sizeA
    
    
    orr   = seq(rate1, rate2, 0.05)
    
    n_range = as.numeric(unlist(strsplit(n, ",")))
    #n_range = c(n-15,n-10,n-5, n, n+5, n+10)    
    n_range = n_range[n_range>1]
    
    map_dfr(1:length(n_range), 
            function(i) decision_table(n_range[i], threshold1, threshold2, 
                                       orr, lrv, tv, a, b)
    ) %>%
      pivot_longer(
        c('P(GO)', 'P(NO-GO)', 'P(INDT)'),
        names_to = "decision",
        values_to = "Probability"
      ) %>%
      ggplot(aes(x=ORR, y=Probability, color = decision)) +
      facet_wrap(~N) + 
      geom_line() +
      geom_point() + 
      geom_vline(xintercept = lrv) + 
      geom_vline(xintercept = tv) + 
      scale_color_manual(values = c("#00AFBB", "gray", "red"))+
      labs(
        title = "Dual-Criteria Plot for Multiple N",
        subtitle =  str_c(" LRV = ", lrv, ", TV = ", tv, ", Threshold1 =",
                          threshold1, ", Threshold2 =", threshold2)
      ) +
      theme_bw()
    
    
  },  height = 700) 
  
  
  output$bayes_decision_two <- renderText({
    # user input 
    TV      = input$tv1
    LRV     = input$lrv1
    prior_a = input$alpha1
    prior_b = input$beta1
    p0      = input$lrv1
    p1      = input$tv1
    a       = input$a_bayesian
    b       = input$b_bayesian
    
    # simon's two stage results 
    simon_results = ph2simon(p0, p1, a, b)
    optimal = simon_results$out[which(simon_results$out[,5] ==min(simon_results$out[,5])),]
    
    # stage one 
    prob1 = posterior(n=optimal[[2]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[1]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>%
      filter(n == optimal[[1]]+1)
    
    # stage two 
    prob2 = posterior(n=optimal[[4]], LRV, TV, prior_a, prior_b) %>%
      mutate(
        Desision = ifelse(n<=optimal[[3]], "No-Go", "Go")
      ) %>% 
      pivot_longer(
        c(TVvec, LRVvec),
        names_to = "posterior_probs",
        values_to = "values"
      ) %>% 
      filter(n == optimal[[3]]+1)
    
    # results 
    first = prob1 %>% filter(posterior_probs == "TVvec")  %>% pull(values) %>% round(4)
    two   = prob1 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    three = prob2 %>% filter(posterior_probs == "TVvec")  %>% pull(values )%>% round(4)
    four  = prob2 %>% filter(posterior_probs == "LRVvec") %>% pull(values) %>% round(4)
    
    # decision boundary 
    paste("From the third tab named: <b> Bayesian Criteria</b> The decision boundaries 
      for the Simon's Two Stage converted to a bayesian design are: 
      <br>
      <h5> <b>For Stage 2 Go/No-Go</b>: Go if P (ORR >",TV ,") ≥", three, " = threshold1, 
      and P (ORR >", LRV,") ≥ ", four,"= threshold2. The suggested sample size is ", optimal[[4]] ,". 
      </h5> If you use values from stage 2 as the sample size and thresholds then 
      the graph should approximate the Simon's two stage design. This is seen by the 
      indecisive gray line going to zero. This is because in the Simon's Two Stage 
      Design there is no indecisive region. However, when you update the thresholds 
      to differ then the indecisive region will appear. This graph explores how the 
      sample size changes the indecisive region.  " 
    )
    
  })
  
  
  #########################################################################################
  #######################   Tab 3, Section 8: Bayesian Exploration   #######################
  #########################################################################################
  output$prior_plot <- renderPlot({
    tv    = input$tv2
    lrv   = input$lrv2
    a     = input$alpha2
    b     = input$beta2
    
    a = as.numeric(unlist(strsplit(a, ",")))
    b = as.numeric(unlist(strsplit(b, ",")))
    
    names = map_chr(1:length(a), function(i) str_c("p~Beta(a=",a[i], ", b=" ,b[i],")"))
    
    p = seq(0, 1, length=100)
    data_prior = map_dfr(
      1:length(a), 
      function(i) data.frame(
        p , 
        prior_vals = dbeta(p, a[i],b[i]), 
        names = rep(names[i], length(p)),
        a = a[i],
        b = b[i]
      )
    )
    
    vline.data <- data_prior %>%
      group_by(names) %>%
      mutate(z = a/(a+b))
    
    data_prior %>% 
      ggplot(aes(x=p, y=prior_vals, color = "#749C75"))+
      geom_line() +
      #geom_point() +
      facet_wrap(~names) + 
      geom_vline(aes(xintercept = z), vline.data,  linetype="dotdash")+
      labs(
        title = str_c("Prior Distribution, p~Beta(a, b)"),
        subtitle = "The Expected Value is displayed by a vertical dotted line", 
        y = "Density",
        x = "Probabilty"
      ) +
      theme_bw()  +
      scale_color_manual(values=c( "#749C75"))
    
    
  }) 
  
  output$math_prior <- output$ex2 <- renderUI({
    withMathJax(
      helpText('The expected value of the beta distribtion is $$E = \\frac{a}{a+b}$$'),
      helpText('The variance of the beta distribution is $$var = \\frac{ab}{(a+b)^2(a+b+1)}$$')
    )
  })
  
  output$likelihood_part1 <- renderPlot({
    tv    = input$tv2
    lrv   = input$lrv2
    a     = input$alpha2
    b     = input$beta2
    n     = input$sample_sizeB
    
    p_vals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    names = map_chr(1:length(p_vals), function(i) str_c("Binomial(n=", n,", p=", p_vals[i], ")"))
    
    p = c(0:(n))
    binomial_dist = map_dfr(1:length(p_vals), function(i) 
      data.frame(
        p, 
        y = dbinom(p, size = n, p_vals[i]), 
        labs = names[i], 
        lrv_col = ifelse(p == round(n*lrv), dbinom(p, size = n, p_vals[i]), NA),
        tv_col = ifelse(p == round(n*tv), dbinom(p, size = n, p_vals[i]), NA)
      )
    )
    
    binomial_dist %>% 
      ggplot()+
      geom_col(aes(x=p, y=y), fill = "lightgray") +
      geom_col(aes(x=p, y=lrv_col), fill = "#E9D985") +
      geom_col(aes(x=p, y=tv_col), fill = "#5D4A66") +
      geom_point(aes(x=p, y=y), color="gray") +
      geom_point(aes(x=p, y=lrv_col), color = "#E9D985") +
      geom_point(aes(x=p, y=tv_col), color = "#5D4A66") +
      facet_wrap(~labs) +
      labs(
        title = str_c("pmf plots of y~Binomial( p, n=", n,") for 9 values of p." ),
        subtitle = "Pink highlight shows Y = LRV*N and Blue highlight shows Y = TV*N ",
        x = "Values",
        y = "Probabilty",
        color = "Reference Value"
      )+
      theme_bw()
    
  }) 
  
  output$likelihood_part2 <- renderPlot({
    tv    = input$tv2
    lrv   = input$lrv2
    a     = input$alpha2
    b     = input$beta2
    n     = input$sample_sizeB
    
    p_vals = seq(0,1,0.01)
    names  = map_chr(1:length(p_vals), function(i) str_c("Binomial(n=", n,", p=", p_vals[i], ")"))
    p      = c(0:(n))
    
    binomial_dist = map_dfr(1:length(p_vals), function(i) 
      data.frame(
        p, 
        y = dbinom(p, size = n, p_vals[i]), 
        p_val = p_vals[i],
        labs = names[i], 
        LRV = ifelse(p == round(n*lrv), dbinom(p, size = n, p_vals[i]), NA),
        TV = ifelse(p == round(n*tv), dbinom(p, size = n, p_vals[i]), NA)
      )
    )
    
    full_join(binomial_dist %>% select(p_val, LRV) %>%drop_na(), 
              binomial_dist %>% select( p_val, TV) %>%drop_na() ) %>%
      pivot_longer(
        cols = c(LRV,TV),
        names_to = "ReferenceValue",
        values_to = "likelihood"
      ) %>%
      ggplot(aes(x=p_val, y = likelihood, color = ReferenceValue))+ 
      geom_point() +
      geom_line() +
      labs(
        title = "Likelihood for Both LRV and TV", 
        subtitle = str_c("for LRV we have L(p|y=", lrv*n,"), and for TV we have L(p|y=", tv*n,")"),
        y = "Likelihood",
        x = "p"
      ) + 
      scale_color_manual(values=c( "#E9D985", "#5D4A66"))
  }) 
  
  output$posterior_part1 <- renderPlot({
    tv    = input$tv2
    lrv   = input$lrv2
    a     = input$alpha4
    b     = input$beta4
    
    a = as.numeric(unlist(strsplit(a, ",")))
    b = as.numeric(unlist(strsplit(b, ",")))
    
    names = map_chr(1:length(a), function(i) str_c("p~Beta(a=",a[i], ", b=" ,b[i],")"))
    n     = input$sample_sizeB
    p     = c(0:n)
    
    post_vals <- function(a,b, names){
      LRVvec1 = TVvec1 = rep(NA,n+1)
      for(y in 0:n){
        LRVvec1[y+1] <- 1-pbeta(lrv, y + a, n - y + b)
        TVvec1[y+1]  <- 1-pbeta(tv, y + a, n - y + b) 
      }
      y = c(0:n)
      vals = c(LRVvec1, TVvec1) 
      lab = c(rep("LRV", length(y)),rep("TV", length(y)))
      return(tibble(y = c(y,y), vals, lab, names = rep(names, length(vals))))
    }
    
    data_post = map_dfr(1:length(a), function(i) post_vals(a[i], b[i], names[i]))
    
    data_post %>% 
      ggplot(aes(x=y, y=vals, color = lab))+
      facet_wrap(~names) +
      geom_line() +
      geom_point() +
      labs(
        title = str_c("Posterior Distribution, p|y~Beta(y+a, n-y-b)"),
        y = "Density",
        x = "Y values",
        color = "Reference Value"
      )+
      theme_bw() +
      scale_color_manual(values=c( "#E9D985", "#5D4A66"))
    
  }) 
  
  output$posterior_part2 <- renderPlot({
    tv    = input$tv2
    lrv   = input$lrv2
    a     = input$alpha3
    b     = input$beta3
    
    names = map_chr(1:length(a), function(i) str_c("p~Beta(a=",a, ", b=" ,b,")"))
    n     = input$sample_sizeB
    
    first = bayesrules::plot_beta_binomial(alpha = a, beta = b, y = round(lrv*n), n = n) +
      labs(
        title = "Posterior Distribution for LRV",
        subtitle = str_c("alpha=", a, ", beta=", b, ", n=", n)
      ) +  
      scale_fill_manual(values=c("#E9D985", "#B2BD7E", "#749C75")) 
    #theme(legend.position = "none") 
    second = bayesrules::plot_beta_binomial(alpha = a, beta = b, y = round(tv*n), n = n) +
      labs(
        title = "Posterior Distribution for TV",
        subtitle = str_c("alpha=", a, ", beta=", b, ", n=", n)
      ) +  scale_fill_manual(values=c("#5D4A66", "#69736E", "#749C75"))
    first+second
    
  }) 
  
  
  #########################################################################################
  #######################   Tab 4, Section 9: Fleming's Two Stage   #######################
  #########################################################################################
  output$panelset_fleming <- renderUI({ # panel set set up
    tries <- input$tries
    m <- matrix(c(15, 20, 20, 25,
                  15, 15, 20, 20),
                tries, 2)
    rownames(m) <- map_chr(1:tries, function(i) str_c("Scenario ", i))
    colnames(m) <- c("sample1", "sample2")
    
    n<-1
    tabList <- lapply(n, function(i) {
      wellPanel(
        h5("Each Scenario's Sample Size"),
        matrixInput("sample_size_fleming",
                    value = m,
                    rows = list( extend = FALSE ),
                    cols = list( extend = FALSE )
        )
      )
    })
    do.call(fluidRow, tabList)
  })
  output$fleming <- DT::renderDataTable({ # table output 
    #user inputs 
    p0    = input$p0_fleming
    pa    = input$pa_fleming
    tries = input$tries
    alpha_fleming = input$alpha_fleming
    
    scenarios <- as_tibble(t(input$sample_size_fleming))  #scenarios <- as_tibble(t(m))
    
    map_dfr(1:tries, function(i)
      fleming_twostage(K=2, p0, pa, as.numeric(scenarios %>% pull(i)), alpha_fleming))
    
  })
  output$fleming_description <- renderText({
    "The null hypothesis that the true response rate is [ p0 ] will be tested against a 
    one-sided alternative. In the first stage, [ n1 ] patients will be accrued, 
    in the second stage [ n2 ] patients will be accrued for a total of [ N ]
    patients. If there are [ a1 ] or fewer responses in these [ n1 ] patients, the study 
    will be stopped for futility. If there are [ r1 ] or more responses in 
    [ n1 ] patients, the study will be stopped and the null hypothesis rejected. 
    The null hypothesis will be rejected if [ r2 ] or more responses are 
    observed in [ n ] patients. This design yields a type I error rate of 
    [ alpha ] and power of [ power ] when the true response 
    rate is [ pa ]."
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)