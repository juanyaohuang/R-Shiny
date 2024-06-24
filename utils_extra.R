# Functions taken from visR package
# Only modification is size -> linewidth in ggplot2::geom_segment

#' Generate cohort attrition table
#'
#' @description
#' 
#' This function calculates the subjects counts excluded and included
#' for each step of the cohort selection process.
#'
#' @param data Dataframe. It is used as the input data to count the subjects
#' that meets the criteria of interest
#' @param criteria_descriptions \code{character} It contains the descriptions
#' of the inclusion/exclusion criteria.
#' Each element of the vector corresponds to the description of each criterion.
#' @param criteria_conditions \code{character} It contains the corresponding
#' conditions of the criteria.
#' These conditions will be used in the table to compute the counts of the
#' subjects.
#' @param subject_column_name \code{character} The column name of the table that
#' contains the subject id.
#'
#' @usage get_attrition(data, criteria_descriptions, criteria_conditions,
#' subject_column_name)
#' @return The counts and percentages of the remaining and excluded subjects
#' for each step of the cohort selection in a table format.
#'
#' @details criteria_descriptions and criteria_conditions need to be of same length
#' 
get_attrition <- function(data,
                          criteria_descriptions,
                          criteria_conditions,
                          subject_column_name) {
  if (!inherits(subject_column_name, "character") || length(subject_column_name) > 1) {
    stop("The 'subject_column_name' argument has to be a string. Please correct the 'subject_column_name' and re-run the function")
  }
  
  if (!subject_column_name %in% names(data)) {
    stop("The 'subject_column_name' argument doesn't correspond to a column name. Please correct the 'subject_column_name' and re-run the function")
  }
  
  if (length(criteria_descriptions) != length(criteria_conditions)) {
    stop("Vectors 'criteria_descriptions' and 'criteria_conditions' must have the same length.")
  }
  
  if (!NA %in% criteria_conditions) {
    criteria_map <- data.frame(cbind(criteria_descriptions, criteria_conditions), stringsAsFactors = FALSE)
    
    final_cond <- c()
    person_count_master <- c()
    
    for (each_cond in criteria_map$criteria_conditions) {
      final_cond <- ifelse(is.null(final_cond),
                           each_cond,
                           paste(paste0("(", final_cond, ")"),
                                 paste0(paste0("(", each_cond), ")"),
                                 sep = " & "
                           )
      )
      person_count_temp <- data %>%
        dplyr::filter(eval(parse(text = final_cond))) %>%
        dplyr::select(!!subject_column_name) %>%
        dplyr::n_distinct()
      
      person_count_master <- c(person_count_master, person_count_temp)
    }
    
    if (length(person_count_master) > 0) {
      count_master_table <- dplyr::tibble("Remaining N" = person_count_master)
      criterion_0 <- dplyr::tibble(
        criteria_conditions = "none",
        criteria_descriptions = "Total cohort size",
        `Remaining N` = dplyr::select(data, !!subject_column_name) %>% dplyr::n_distinct()
      )
      
      # generate attrition table
      attrition_table <-
        criterion_0 %>%
        dplyr::bind_rows(cbind(criteria_map, count_master_table)) %>%
        dplyr::mutate(
          `Remaining %` = 100 * `Remaining N` / max(`Remaining N`),
          `Excluded N` = dplyr::lag(`Remaining N`, n = 1L, default = max(`Remaining N`)) - `Remaining N`,
          `Excluded %` = 100 * `Excluded N` / max(`Remaining N`)
        ) %>%
        # rename columns
        dplyr::rename(
          Condition = criteria_conditions,
          Criteria = criteria_descriptions
        ) %>%
        # fix formatting
        dplyr::select(Criteria, Condition, dplyr::everything())
      
      class(attrition_table) <- c("attrition", class(attrition_table))
      return(attrition_table)
    }
  }
}

#' @description
#' 
#' S3 function to draw a Consort flow diagram chart.
#' 
visr <- function(x,
                 description_column_name = "Criteria",
                 value_column_name = "Remaining N",
                 complement_column_name = "",
                 box_width = 50,
                 font_size = 12,
                 fill = "white",
                 border = "black",
                 ...) {
  if (!description_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", description_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input ",
      "data containing the inclusion descriptions."
    ))
  }
  
  if (!value_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", value_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input data containing",
      "the sample size after applying inclusion criteria."
    ))
  }
  
  if (complement_column_name != "" & !complement_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", complement_column_name, "\" cannot be found in the input data. ",
      "Please provide a valid column name as string in the input data containing",
      "complement description or omit this argument for default labels."
    ))
  }
  
  if (!is.numeric(box_width)) {
    warning("An invalid input was given for `box_width`, must be `numeric` value. Setting it to 50.")
    box_width <- 50
  }
  
  if (!is.numeric(font_size)) {
    warning("An invalid input was given for `font_size`, must be `numeric` value. Setting it to 12.")
    font_size <- 12
  }
  
  if (!is.character(fill)) {
    warning("An invalid input was given for `fill`, must be `character` string. Setting it to \"white\".")
    fill <- "white"
  }
  
  if (!is.character(border)) {
    warning("An invalid input was given for `border`, must be `character` string. Setting it to \"black\".")
    border <- "black"
  }
  
  label <- complement_label <- NULL
  y <- down_ystart <- down_yend <- side_xstart <- side_xend <- side_y <- NULL
  cx <- cy <- NULL
  
  # split up space into evenly sized chunks
  field_height <- 100 / nrow(x)
  
  # allow for some spacing between boxes by reducing the size of the chunk
  box_height <- 0.75 * field_height
  
  # assign coordinates to each row in the attrition table
  plotting_data <- x %>%
    .get_labels(description_column_name, value_column_name, complement_column_name, wrap_width = box_width) %>%
    .get_labelsizes(label, complement_label) %>%
    .get_coordinates(box_width, box_height, field_height)
  
  # draw plot
  gg <- plotting_data %>%
    ggplot2::ggplot() +
    # boxes
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill
    ) +
    # text in boxes
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        label = label
      ),
      size = font_size / ggplot2::.pt
    ) +
    # down arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = x,
        xend = x,
        y = down_ystart,
        yend = down_yend
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      linewidth = .2,
      na.rm = TRUE
    ) +
    # side arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = side_xstart,
        xend = side_xend,
        y = side_y,
        yend = side_y
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      linewidth = .2,
      na.rm = TRUE
    ) +
    # complement box
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill,
      na.rm = TRUE
    ) +
    # text in complement box
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        label = complement_label
      ),
      size = font_size / ggplot2::.pt,
      na.rm = TRUE
    ) +
    # remove all plot elements
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  
  return(gg)
}

#' @title Create labels for flowchart
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe
#' @param description_column_name \code{character} The column name containing description part of the new label
#' @param value_column_name \code{character} The column name containing the sample size part of the new label
#' @param complement_column_name \code{character} The column name containing a complement description part (will result in a second label)
#' @param wrap_width \code{integer} for the maximal character count per line
#'
#' @return The input dataframe extended by two columns containing the label and complement label
#' 
.get_labels <- function(data, description_column_name, value_column_name, complement_column_name = "", wrap_width = 50) {
  label <- complement_label <- NULL
  
  plotting_data <- data %>%
    dplyr::rowwise() %>%
    # below needs update to description_column_name instead of Criteria
    dplyr::mutate(label = paste(strwrap(get(description_column_name), width = wrap_width), collapse = "\n")) %>%
    # below needs update to value_column_name instead of `Remaining N`
    dplyr::mutate(label = sprintf("%s\nN = %d", label, get(value_column_name)))
  
  
  if (complement_column_name != "") {
    plotting_data <- plotting_data %>%
      # below needs update to complement_column_name instead of Complement
      dplyr::mutate(complement_label = paste(strwrap(get(complement_column_name), width = wrap_width), collapse = "\n")) %>%
      dplyr::ungroup() %>%
      # below needs update to value_column_name instead of `Remaining N`
      dplyr::mutate(complement_label = sprintf(
        "%s\nN = %d",
        complement_label,
        dplyr::lag(get(value_column_name)) - get(value_column_name)
      ))
  } else {
    plotting_data <- plotting_data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(complement_label = sprintf(
        "%s N = %d", "Excluded",
        dplyr::lag(get(value_column_name)) - get(value_column_name)
      ))
  }
  
  return(plotting_data)
}

#' @title Calculate the size labels on the
#'
#' @description Calculate the text width and maximal text width of both the label and complement labels
#'
#' @param data Dataframe with label and complement label strings
#' @param label The column containing attrition labels
#' @param complement_label The column containing complement description labels
#'
#' @return The input dataframe extended by several columns containing the label and complement label height and width
#'
.get_labelsizes <- function(data, label, complement_label) {
  labelheight <- labelwidth <- complementheight <- complementwidth <- maxwidth <- maxheight <- NULL
  
  plotting_data <- data %>%
    dplyr::mutate(
      labelwidth = graphics::strwidth({{ label }}, units = "inch"),
      complementwidth = graphics::strwidth({{ complement_label }}, units = "inch"),
      maxwidth = max(labelwidth, complementwidth),
      labelheight = graphics::strheight({{ label }}, units = "inch"),
      complementheight = graphics::strheight({{ complement_label }}, units = "inch"),
      maxheight = max(labelheight, complementheight)
    ) %>%
    dplyr::select(labelwidth, complementwidth, maxwidth, labelheight, complementheight, maxheight, dplyr::everything())
  return(plotting_data)
}

#' @title Create coordinates for each row in the attrition table
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe containing the attrition data
#' @param box_width \code{integer} The width of the boxes in the flow charts (in canvas coordinates)
#' @param box_height \code{integer} The height of the boxes in the flow charts (in canvas coordinates)
#' @param field_height \code{float} The width of the boxes in the flow charts (in canvas coordinates)
#'
#' @return The input dataframe extended by columns containing x and y coordinates for included and excluded counts
#'
.get_coordinates <- function(data, box_width, box_height, field_height) {
  y <- ymin <- ymax <- down_ystart <- down_yend <- x <- side_xend <- side_y <- NULL
  
  plotting_data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      x = 50,
      y = 100 - dplyr::row_number() * field_height + box_height / 2
    ) %>%
    # coordinates of text box
    dplyr::mutate(
      box_width = box_width,
      box_height = box_height,
      ymin = y - (box_height / 2),
      ymax = y + (box_height / 2)
    ) %>%
    # coordinates of down arrow
    dplyr::mutate(
      down_ystart = dplyr::lag(ymin),
      down_yend = ymax
    ) %>%
    # coordinates of side arrow
    dplyr::mutate(
      side_y = down_ystart - 0.5 * (down_ystart - down_yend),
      side_xstart = x,
      side_xend = x + (box_width / 2) + 10
    ) %>%
    # complement coordinates
    dplyr::mutate(
      cx = side_xend + (box_width / 2),
      cy = side_y
    )
  
  return(plotting_data)
}
