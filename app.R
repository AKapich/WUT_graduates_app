library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(tidyr)


# Zmapowanie pełnych nazw wydziałów PW na ich skrótowe odpowiedniki
faculty_map = list(
  "Wydział Matematyki i Nauk Informacyjnych" = "[MiNI]",
  "Wydział Elektryczny" = "[WE]",
  "Wydział Elektroniki i Technik Informacyjnych" = "[EiTI]",
  "Wydział Mechatroniki" = "[WM]",
  "Wydział Mechaniczny Energetyki i Lotnictwa" = "[MEiL]",
  "Wydział Mechaniczny Technologiczny" = "[WMT]",
  "Wydział Geodezji i Kartografii" = "[GiK]",
  "Wydział Zarządzania" = "[WZ]",
  "Wydział Samochodów i Maszyn Roboczych" = "[SiMR]"
)

# tworzymy customową paletę kolorów do wykresu
paleta = setNames(object = c("firebrick2", "green3", "cadetblue3",
                             "slateblue3", "yellow3", "seagreen",
                             "mistyrose4", "red2", "hotpink"),
                  nm = paste(unlist(faculty_map),
                             names(faculty_map),
                             sep=" - "))


df_graduates <- read.csv("./graduates.csv")[2:688]

# reszta przekształcania danych dla 1 wykresu
df <- df_graduates %>%
  group_by(Faculty=P_NAZWA_JEDN, Career=P_KIERUNEK_NAZWA) %>%
  summarise(Zarobki.Ndosw = mean(P_E_ZAR_ETAT_NDOSW),
            Zarobki.Dosw = mean(P_E_ZAR_ETAT_DOSW),
            Zarobki.Wszyscy = mean(P_E_ZAR_ETAT)) %>%
  na.omit()

df$Faculty <- str_replace_all(df$Faculty,
                              c("Ĺ‚"="ł", "Ăł"="ó", "Ä…"="ą", "ĹĽ"="ż"))
df$Career <- str_replace_all(df$Career,
                             c("Ĺ‚"="ł", "Ăł"="ó", "Ä…"="ą", "ĹĽ"="ż"))
df <- df %>% mutate(ID=paste(faculty_map[Faculty], Career, sep=" "),
                    Legenda=paste(faculty_map[Faculty], Faculty, sep=" - "))



# reszta przekształcania danych dla 2 wykresu
df2 <- df_graduates %>%
  select(Faculty = P_NAZWA_JEDN,
         c(551:586)) %>%
  na.omit() %>% 
  group_by(Faculty) %>% 
  summarize(across(cols=everything(), .fns = mean))

df2$Faculty <- str_replace_all(df2$Faculty,
                          c("Ĺ‚"="ł", "Ăł"="ó", "Ä…"="ą", "ĹĽ"="ż", "Ĺš"="Ś"))

df2 <- as.data.frame(t(df2))
df2 <- janitor::row_to_names(df2, 1)

################################################################
#                        APLIKACJA                            #
################################################################

ui1 <- fluidPage(
    titlePanel("Średnie miesięczne wynagrodzenie
               z tytułu umów o pracę (po uzyskaniu dyplomu)
               wsród absolwentów PW."),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId= "experience",
                        "Doświadczenie absolwentów w pracy z okresu
                        przed uzyskaniem dyplomu:",
                        choices=c("Bez doświadczenia"=0,
                                  "Z doświadczeniem"=1,
                                  "Wszyscy"=2),
                        selected=0),
            shiny::markdown(
              "Dane dotyczą tylko absolwentów studiów stacjonarnych
       pierwszego stopnia, którzy otrzymali dyplom w roku 2017
       bądź później."
            )
        ),

        mainPanel(
          shinycssloaders::withSpinner(
            shinyjqui::jqui_resizable(plotOutput("barPlot")),
            type = 4,
            color = "darkblue",
            size=0.5)
        )
    )
)

ui2 <- fluidPage(
  titlePanel("Względny Wskaźnik Zarobków w kolejnych miesiącach
  po uzyskaniu dyplomu wśród absolwentów PW (względem wydziałów)."),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "faculty",
                  "Wydział PW:",
                  choices=colnames(df2),
                  selected="Wydział Matematyki i Nauk Informacyjnych"),
      shiny::markdown(
        "Dane dotyczą tylko absolwentów studiów stacjonarnych
       pierwszego stopnia, którzy otrzymali dyplom w roku 2017
       bądź później.
       
       **Uwaga!** Ze względu na nieuwzględnienie roczników dla
       których zabrakło danych dla wszystkich 36 miesięcy, niektóre wykresy
       generowane są przy niewielkiej próbce danych. Należy brać to pod uwagę
       podczas odczytywania informacji.
      
       Względny Wskaźnik Zarobków to średnia wartość ilorazu średniego
       miesięcznego wynagrodzenia absolwenta do średniego miesięcznego
       wynagrodzenia w powiecie zamieszkanym przez absolwenta."
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotOutput("scatterPlot")),
        type = 4,
        color = "aliceblue",
        size=0.5)
    )
  )
)

ui <- navbarPage(
  title = "Zarobki absolwentów PW",
  tabPanel("Średnie zarobki", ui1, icon = icon("dollar-sign")),
  tabPanel("Względny Wskaźnik Zarobków", ui2, icon = icon("compass")),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2022 Autor:
                    <a class='text-dark' href='https://github.com/AKapich'>Aleks Kapich</a>
                  </p>
                  </footer>
                  "),
  theme = bslib::bs_theme(bootswatch = "superhero")
  )

server <- function(input, output) {

    output$barPlot <- renderPlot({
      # Wybranie odpowiednich danych w zależności od inputu
      df <- df[c(1,2,3+as.numeric(input$experience),6,7)]
      colnames(df)[3] <- "Zarobki"
      df <- df %>% 
        arrange(desc(Zarobki)) %>%
        head(20)
      
      # Stworzenie wykresu
      ggplot(data=df, aes(x= reorder(ID, Zarobki),
                          y=Zarobki,
                          fill=Legenda)) +
        geom_col() +
        scale_y_continuous(expand=c(0,0),
                         limits=c(0,1.1*max(df$Zarobki))
                         ) +
        scale_fill_manual(values = paleta) +
        labs(
          x = "Kierunek",
          y = "Zarobki [PLN]",
          title = "Top 20 kierunków"
        ) +
        coord_flip() +
        theme_minimal()
    })
    
    
    output$scatterPlot <- renderPlot({
      # Wybranie odpowiednich danych w zależności od Inputu
      df<- df2[input$faculty]
      colnames(df)[1] <- "Faculty"
      df["Index"] <- c(1:nrow(df))
      
      # Stworzenie wykresu
      ggplot(data=df, aes(x=Index, y=as.numeric(Faculty)))+
        geom_point()+
        scale_x_discrete(expand=c(0,0),
                           limits=factor(1:36)) +
        scale_y_continuous(limits=c(0.25, 1.75)) +
        labs(x="Miesiąc po uzyskaniu dyplomu",
             y=paste("WWZ", input$faculty, sep=" - "),
             title="Względny Wskaźnik Zarobków (WWZ) w kolejnych miesiącach po otrzymaniu dyplomu")+
        theme_minimal() +
        geom_smooth(method="lm")
    })
}

shinyApp(ui = ui, server = server)
