#Last edited by - Deus Thindwa
#Date - 28/10/2019

# simulate  social contact data
somipa.mat <- somipa

## check if dataset is formatted for working with contact matrices
check(
  x,
  columns = FALSE,
  quiet = FALSE,
  error = FALSE,
  id.column = "part_id",
  participant.age.column = "part_age",
  country.column = "country",
  year.column = "year",
  contact.age.column = "cnt_age",
  ...
)

# create a survey object
data(polymod)
check(polymod)
cite(polymod)
cleaned <- clean(polymod)


new_survey <- survey(polymod$participants, polymod$contacts)











