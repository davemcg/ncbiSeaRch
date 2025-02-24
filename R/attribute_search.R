#' @title Attribute Finder
#' @description This function takes an XML list object and extracts the values of a specific attribute.
#' @param xml_list_obj A list object containing XML data.
#' @return A list of attribute values.
#' @keywords internal
attribute_finder <- function(xml_list_obj) {
  out <- list()
  for (i in 1:length(xml_list_obj)) {
    if (!is.null(xml_list_obj[[i]]$.attrs["attribute_name"])) {
      out[[i]] <- xml_list_obj[[i]]$.attrs["attribute_name"]
    } else {
      out[[i]] <- NA
    }
  }
  return(out)
}


#' @title Value Grabber
#' @description This function takes an attribute and an XML list object, and returns the text value associated with the attribute.
#' @param attribute A character string representing the attribute name.
#' @param xml_list_obj A list object containing XML data.
#' @return A character string representing the text value of the attribute.
#' @keywords internal
value_grabber <- function(attribute, xml_list_obj) {
  for (i in 1:length(xml_list_obj)) {
    if (attribute %in% (xml_list_obj[[i]]$.attrs)) {
      out <- xml_list_obj[[i]]$text
      return(out)
    }
  }
}

#' Create Attribute DataFrame from NCBI BioSample
#'
#' This function fetches XML data from NCBI BioSample using a provided ID, extracts attributes from the XML,
#' and constructs a data frame containing these attributes along with their values.
#'
#' @param id A character string representing the NCBI BioSample ID.
#' @return A data frame with columns for attribute names, their corresponding values, and the BioSample ID.
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- attribute_df_maker("SAMN12345678")
#' print(result)
#' }
#' @export
attribute_df_maker <- function(id) {
  # fetch xml object from NCBI
  eutil_grab <- efetch(uid = id, db = "biosample", retmode = "xml")
  # extract attributes, convert to list
  xml_list_obj <- eutil_grab[["//Attributes"]] %>% XML::xmlToList()
  biosample_title <- (eutil_grab[["//Description"]] %>% XML::xmlToList())$Title
  # scan through list obj and find all attribures
  attribute_df <- attribute_finder(xml_list_obj) %>%
    as.character() %>%
    data.frame()
  colnames(attribute_df)[1] <- "attribute"
  # grab the attributes and stick into DF
  attribute_df <- attribute_df %>%
    rowwise() %>%
    mutate(value = value_grabber(attribute, xml_list_obj))
  attribute_df$id <- id
  attribute_df <- bind_rows(attribute_df, c(attribute = "biosample_title", value = biosample_title, "id" = id))
  return(attribute_df)
}
