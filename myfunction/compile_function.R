compile_function <-
  function(data,
           common_fields,
           a_list,
           b_list,
           c_list,
           d_list,
           e_list) {
    datadf <- data %>%
      dplyr::select(common_fields,
                    a_list,
                    b_list,
                    c_list,
                    d_list,
                    e_list)
    return(datadf)
  }
