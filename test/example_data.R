# Shared example data for every test in test/ (also used in geo_site.qmd).
# These are ten PUBLIC state capitol buildings, never participant data. A
# capitol's address is not linked to any person.
#
# The states were picked to cover every region and to include five states whose
# FIPS codes start with 0 (AL 01, AZ 04, CA 06, CO 08, CT 09), which tests that
# leading zeros are kept. Connecticut also tests the 2022 planning-region fix.
#
# Each address was checked against the Census geocoder, and every match falls
# within about 150 m of the capitol building. The coordinates and expected
# values come from the Census geocoder + geo_2020().
# Zip codes are text so Connecticut's leading zero (06106) survives.

library(tibble)

example_addresses <- tibble(
  street = c("600 Dexter Ave",        # Alabama State Capitol
             "1700 W Washington St",  # Arizona State Capitol
             "1315 10th St",          # California State Capitol
             "200 E Colfax Ave",      # Colorado State Capitol
             "210 Capitol Ave",       # Connecticut State Capitol
             "206 Washington St SW",  # Georgia State Capitol
             "401 S 2nd St",          # Illinois State Capitol
             "900 Court St NE",       # Oregon State Capitol
             "501 N 3rd St",          # Pennsylvania State Capitol
             "1100 Congress Ave"),    # Texas State Capitol
  city   = c("Montgomery", "Phoenix", "Sacramento", "Denver", "Hartford",
             "Atlanta", "Springfield", "Salem", "Harrisburg", "Austin"),
  state  = c("AL", "AZ", "CA", "CO", "CT", "GA", "IL", "OR", "PA", "TX"),
  zip    = c("36130", "85007", "95814", "80203", "06106",
             "30334", "62701", "97301", "17120", "78701")
)

# Census geocoder coordinates for the addresses above
example_coords <- tibble(
  id    = 1:10,
  state = example_addresses$state,
  lat   = c(32.377613539725, 33.448174841255, 38.576770901007, 39.739996725077,
            41.762863585706, 33.749798156190, 39.798337678856, 44.939049633207,
            40.263320403715, 30.276441992220),
  lon   = c(-86.301882734148, -112.098251192919, -121.494921447183, -104.984403184626,
            -72.682254053971, -84.388565089734, -89.653455817947, -123.030567446723,
            -76.884362089960, -97.739754035328)
)

# ADI is NA for AL, AZ, GA, and OR: those capitol block groups have suppressed
# ADI ranks (few residents), which also tests NA handling.
example_expected <- tibble(
  geoid_block = c("011010002001", "040131143011", "060670011033", "080310027041",
                  "090035021002", "131210035001", "171670013002", "410470002003",
                  "420430201001", "484530007001"),
  svi_tract   = c(0.9523, 0.7490, 0.7278, 0.4302, 0.5707,
                  0.6613, 0.6385, 0.6978, 0.5814, 0.4776),
  adi_nat     = c(NA, NA, 35, 12, 64, NA, 93, NA, 68, 22),
  adi_state   = c(NA, NA, 9, 2, 9, NA, 10, NA, 6, 1)
)
