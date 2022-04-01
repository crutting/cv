library(ggplot2)
library(here)
library(tidyverse)
library(utils)
library(data.table)
library(plotly)


plot_timeline2 <- function(event, start, end = start + 1, label = NA, group = NA,
                           title = "Curriculum Vitae Timeline", subtitle = "Clark Ruttinger",
                           size = 7, colour = "orange", save = FALSE, subdir = NA) {
  df <- data.frame(
    Role = as.character(event), Place = as.character(label),
    Start = lubridate::date(start), End = lubridate::date(end),
    Type = group
  )
  cvlong <- data.frame(pos = rep(
    as.numeric(rownames(df)),
    2
  ), name = rep(as.character(df$Role), 2), type = rep(factor(df$Type,
                                                             ordered = TRUE
  ), 2), where = rep(
    as.character(df$Place),
    2
  ), value = c(df$Start, df$End), label_pos = rep(df$Start +
                                                    floor((df$End - df$Start) / 2), 2))
  maxdate <- max(df$End)
  p <- ggplot(cvlong, aes(
    x = value, y = reorder(name, -pos),
    label = where, group = pos
  )) + geom_vline(
    xintercept = maxdate,
    alpha = 0.8, linetype = "dotted"
  ) + labs(
    title = title,
    subtitle = subtitle, x = NULL, y = NULL, colour = NULL
  )+ theme(
    #axis.text=element_text(size=2), 
    #axis.title=element_text(size=2), 
    #plot.title=element_text(size=2), 
    #legend.text=element_text(size=2), 
    #legend.title=element_text(size=2)
    text=element_text(size=10))   

  if (!is.na(cvlong$type)[1] | length(unique(cvlong$type)) >
      1) {
    p <- p + geom_line(aes(color = type), size = size) +
      facet_grid(type ~ ., scales = "free", space = "free") +
      guides(colour = "none") +
      scale_colour_hue()
  }
  else {
    p <- p + geom_line(size = size, colour = colour)
  }
  p <- p + geom_label(aes(x = label_pos),
                      colour = "black",
                      size = 2, alpha = 0.7
  )
  if (save) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep = "/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  return(p)
}

order <- c("Role", "Place", "Type", "Start", "End")
today <- as.character(Sys.Date())


### Edit from here ###
cv <- data.frame(rbind(
c("Certificate- Information Systems", "University of Utah", "Academic", "2016-05-01", "2017-05-01"),
c("Master of Business Administration", "University of Utah", "Academic", "2015-05-01", "2017-05-01"),
c("Certified Mediator- 40 hours", "Utah State Bar Association", "Academic", "2015-09-01", "2015-09-01"),
c("Master of Public Administration", "University of Utah", "Academic", "2005-05-01", "2007-05-01"),
c("BA- Spanish", "University of Utah", "Academic", "1998-05-01", "2003-05-01"),
c("BA- Political Science", "University of Utah", "Academic", "1998-05-01", "2003-05-01"),
c("Certificate- International Relations", "University of Utah", "Academic", "1998-05-01", "2003-05-01"),
c("Intern- Hinkley Institute of Politics", "University of Utah", "Academic", "2000-05-01", "2000-8-31"),
c("Director- Workforce Research","Utah Medical Education Council", "Work Experience", "2018-07-01", today),
c("Director- Utah Nursing Workforce Information Center", "Utah Medical Education Council", "Work Experience", "2015-07-01", today),
c("Research Consultant", "Utah Medical Education Council", "Work Experience", "2010-01-10", "2015-07-01"),
c("Respite Care Provider", "Valley Mental Health", "Work Experience", "2009-07-01", "2017-07-01"),
c("Administrative Hearing Specialist", "Utah Department of Workforce Services", "Work Experience", "2005-07-05", "2009-12-31"),
c("Grants Coordinator", "The Road Home", "Work Experience", "2003-10-01", "2005-01-01"),
c("Americorps- Care Coordinator", "Wasatch Homeless Health- Fourth St. Clinic", "Work Experience", "2002-05-01", "2002-10-01"),
c("President Elect", "National Forum of State Nursing Workforce Centers", "Volunteer Experience", "2018-06-01", "2019-06-01"),
c("Board President", "National Forum of State Nursing Workforce Centers", "Volunteer Experience", "2019-06-01", "2020-06-01"),
c("Immediate Past President", "National Forum of State Nursing Workforce Centers", "Volunteer Experience", "2020-06-01", "2021-06-01"),
c("Planning Commissioner", "Salt Lake City Planning Commission", "Volunteer Experience", "2011-11-01", "2019-07-01"),
c("Committee Chair", "Salt Lake City Planning Commission", "Volunteer Experience", "2015-09-01", "2016-09-01"),
c("Grant Reviewer", "U.S. Department of Health and Human Services-Health Resources and Services Division", "Volunteer Experience", "2011-01-01", today),
c("Landlord- Tenant Mediator", "Community Action Program", "Volunteer Experience", "2009-01-01", "2009-12-31")
))



colnames(cv) <- order
colour <- c("red", "blue", "green")


