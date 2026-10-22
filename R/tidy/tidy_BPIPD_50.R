#' Tidier for BPIPD-50 (NLSY79 Child and Young Adult)
#'
#' One wide NLS Investigator extract headed by reference numbers; the loader
#' script supplies labels and column maps slice out and stack each survey year.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per survey year (1990-2016) with at least one
#'   carried measurement
tidy_BPIPD_50 <- function(raw_dataset, spec) {
  # `biggerpicture.dat` is the same extract without a header row.
  raw <- raw_dataset$data[["Main Dataset (CSV Format)"]]
  dictionary <- bp50_dictionary(raw_dataset$docs)
  decimals <- bp50_implied_decimals(raw_dataset$codebook)

  constants <- bp50_constants()
  fixed <- tibble::as_tibble(lapply(
    constants,
    function(rnum) bp50_zap(raw[[rnum]])
  ))

  items <- bp50_item_map()
  scores <- bp50_score_stems()
  markers <- setdiff(c(names(items), scores), bp50_administrative_stems())

  stacked <- dplyr::bind_rows(lapply(bp50_years(), function(year) {
    bp50_year_frame(
      raw,
      fixed,
      year,
      bp50_year_sources(year, dictionary, items, scores),
      markers,
      decimals
    )
  }))

  stacked <- stacked[intersect(
    c(names(constants), "wave", names(items), scores),
    names(stacked)
  )]

  if (anyDuplicated(stacked[c("CPUBID", "wave")]) > 0L) {
    stop(
      "BPIPD-50: `CPUBID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp50_apply_labels(stacked, dictionary, constants, items, scores, decimals)
}

#' Survey years kept, in collection order
bp50_years <- function() {
  # 1995 is a school-administrator survey; 2018 and 2020 field no screen item.
  # 1986 and 1988 field only household TV-on hours, which do not keep a wave.
  c(
    1990L,
    1992L,
    1994L,
    1996L,
    1998L,
    2000L,
    2002L,
    2004L,
    2006L,
    2008L,
    2010L,
    2012L,
    2014L,
    2016L
  )
}

#' Columns that describe the child once, not once per survey year
bp50_constants <- function() {
  c(
    CPUBID = "C0000100",
    MPUBID = "C0000200",
    CRACE = "C0005300",
    CSEX = "C0005400",
    CYRB = "C0005700",
    NUMYAINTVS = "Y1205200",
    VERSION_R29 = "Y2267000"
  )
}

#' Assessment scores and flags named by stem plus survey year (`ANTI1986`)
bp50_score_stems <- function() {
  c(
    "ACTVTY",
    "ANTI",
    "ANTIP",
    "ANTIPX",
    "ANTIZ",
    "ANTIZX",
    "ANX",
    "ANXP",
    "ANXPX",
    "ANXZ",
    "ANXZX",
    "BPEXTP",
    "BPEXTR",
    "BPEXTS",
    "BPEXTZ",
    "BPI",
    "BPIENRL",
    "BPINTP",
    "BPINTR",
    "BPINTS",
    "BPINTZ",
    "BPIP",
    "BPIPX",
    "BPIZ",
    "BPIZX",
    "BPMIS2",
    "BPMIS3",
    "BPMISS",
    "BPTOTP",
    "BPTOTR",
    "BPTOTZ",
    "CINTRV",
    "COMP",
    "COMPP",
    "COMPZ",
    "DEP",
    "DEPP",
    "DEPPX",
    "DEPZ",
    "DEPZX",
    "DIGITZ",
    "HEAD",
    "HEADP",
    "HEADPX",
    "HEADZ",
    "HEADZX",
    "HYPR",
    "HYPRP",
    "HYPRPX",
    "HYPRZ",
    "HYPRZX",
    "MATH",
    "MATHP",
    "MATHZ",
    "NEGATV",
    "PEER",
    "PEERP",
    "PEERPX",
    "PEERZ",
    "PEERZX",
    "RECOG",
    "RECOGP",
    "RECOGZ",
    "YAINTV"
  )
}

#' Fieldwork flags, released for every child, that do not keep a row
bp50_administrative_stems <- function() {
  c("BPIENRL", "BPMIS2", "BPMIS3", "BPMISS", "CINTRV", "YAINTV")
}

#' One column-map entry: a label and the reference numbers by survey year
bp50_item <- function(label, ...) {
  list(label = label, years = list(...))
}

#' Source reference numbers for each survey item, by survey year
bp50_item_map <- function() {
  # One response format per stem; bands, hours and HOME age forms stay apart.
  list(
    home_a_tv_hours_wd = bp50_item(
      "Mother report, HOME part A (ages 0-2): hours the child watches TV on a typical weekday",
      "1990" = "C0953000",
      "1992" = "C1151100",
      "1994" = "C1401900",
      "1996" = "C1603600",
      "1998" = "C1944100",
      "2000" = "C2264800",
      "2002" = "C2709600",
      "2004" = "C2986200",
      "2006" = "C3388700",
      "2008" = "C3914500",
      "2010" = "C5166000",
      "2012" = "C5742200",
      "2016" = "C6081500"
    ),
    home_a_tv_hours_we = bp50_item(
      "Mother report, HOME part A (ages 0-2): hours the child watches TV on a typical weekend day",
      "1990" = "C0953100",
      "1992" = "C1151200",
      "1994" = "C1402000",
      "1996" = "C1603700",
      "1998" = "C1944200",
      "2000" = "C2264900",
      "2002" = "C2709700",
      "2004" = "C2986300",
      "2006" = "C3388800",
      "2008" = "C3914600",
      "2010" = "C5166100",
      "2012" = "C5742300",
      "2016" = "C6081600"
    ),
    home_b_tv_hours_wd = bp50_item(
      "Mother report, HOME part B (ages 3-5): hours the child watches TV on a typical weekday",
      "1990" = "C0956200",
      "1992" = "C1154300",
      "1994" = "C1405500",
      "1996" = "C1607200",
      "1998" = "C1947800",
      "2000" = c("C2268100", "C2419500"),
      "2002" = "C2712900",
      "2004" = "C2989300",
      "2006" = "C3391800",
      "2008" = "C3917700",
      "2010" = "C5168900",
      "2012" = "C5745400",
      "2014" = "C6015300",
      "2016" = "C6084000"
    ),
    home_b_tv_hours_we = bp50_item(
      "Mother report, HOME part B (ages 3-5): hours the child watches TV on a typical weekend day",
      "1990" = "C0956300",
      "1992" = "C1154400",
      "1994" = "C1405600",
      "1996" = "C1607300",
      "1998" = "C1947900",
      "2000" = c("C2268200", "C2419600"),
      "2002" = "C2713000",
      "2004" = "C2989400",
      "2006" = "C3391900",
      "2008" = "C3917800",
      "2010" = "C5169000",
      "2012" = "C5745500",
      "2014" = "C6015400",
      "2016" = "C6084100"
    ),
    home_c_tv_hours_wd = bp50_item(
      "Mother report, HOME part C (ages 6-9): hours the child watches TV on a typical weekday",
      "1990" = "C0958500",
      "1992" = "C1156600",
      "1994" = "C1408200",
      "1996" = "C1609800",
      "1998" = "C1950500",
      "2000" = "C2422300",
      "2002" = "C2715900",
      "2004" = "C2992200",
      "2006" = "C3394700",
      "2008" = "C3920600",
      "2010" = "C5171800",
      "2012" = "C5748300",
      "2014" = "C6018200",
      "2016" = "C6086900"
    ),
    home_c_tv_hours_we = bp50_item(
      "Mother report, HOME part C (ages 6-9): hours the child watches TV on a typical weekend day",
      "1990" = "C0958600",
      "1992" = "C1156700",
      "1994" = "C1408300",
      "1996" = "C1609900",
      "1998" = "C1950600",
      "2000" = "C2422400",
      "2002" = "C2716000",
      "2004" = "C2992300",
      "2006" = "C3394800",
      "2008" = "C3920700",
      "2010" = "C5171900",
      "2012" = "C5748400",
      "2014" = "C6018300",
      "2016" = "C6087000"
    ),
    home_d_tv_hours_wd = bp50_item(
      "Mother report, HOME part D (ages 10-14): hours the child watches TV on a typical weekday",
      "1990" = "C0963500",
      "1992" = "C1161700",
      "1994" = "C1413700",
      "1996" = "C1615300",
      "1998" = "C1956100",
      "2000" = "C2428200",
      "2002" = "C2721200",
      "2004" = "C2997300",
      "2006" = "C3399800",
      "2008" = "C3925700",
      "2010" = "C5176900",
      "2012" = "C5753400",
      "2014" = "C6023300",
      "2016" = "C6091900"
    ),
    home_d_tv_hours_we = bp50_item(
      "Mother report, HOME part D (ages 10-14): hours the child watches TV on a typical weekend day",
      "1990" = "C0963600",
      "1992" = "C1161800",
      "1994" = "C1413800",
      "1996" = "C1615400",
      "1998" = "C1956200",
      "2000" = "C2428300",
      "2002" = "C2721300",
      "2004" = "C2997400",
      "2006" = "C3399900",
      "2008" = "C3925800",
      "2010" = "C5177000",
      "2012" = "C5753500",
      "2014" = "C6023400",
      "2016" = "C6092000"
    ),
    home_a_tv_on_hours = bp50_item(
      "Mother report, HOME part A (ages 0-2): hours the television is on each day in the home",
      "1990" = "C0953200",
      "1992" = "C1151300",
      "1994" = "C1402100",
      "1996" = "C1603800",
      "1998" = "C1944300",
      "2000" = "C2265100",
      "2002" = "C2709900",
      "2004" = "C2986400",
      "2006" = "C3388900",
      "2008" = "C3914700",
      "2010" = "C5166200",
      "2012" = "C5742400",
      "2016" = "C6081700"
    ),
    home_b_tv_on_hours = bp50_item(
      "Mother report, HOME part B (ages 3-5): hours the television is on each day in the home",
      "1990" = "C0955000",
      "1992" = "C1153100",
      "1994" = "C1404300",
      "1996" = "C1605900",
      "1998" = "C1946500",
      "2000" = c("C2267600", "C2418100"),
      "2002" = "C2712400",
      "2004" = "C2988700",
      "2006" = "C3391200",
      "2008" = "C3917100",
      "2010" = "C5168300",
      "2012" = "C5744800",
      "2014" = "C6014700",
      "2016" = "C6083400"
    ),
    child_tv_hours_wd = bp50_item(
      "Child self-report (age 10+): hours watching TV on a typical weekday",
      "1990" = "C0941500",
      "1992" = "C1136300",
      "1994" = "C1365400",
      "1996" = "C1586900",
      "1998" = "C1925700",
      "2000" = "C2464000",
      "2002" = "C2765400",
      "2004" = "C3045000",
      "2006" = "C3365500",
      "2008" = "C3869200",
      "2010" = "C5117400",
      "2012" = "C5694700",
      "2014" = "C5966600"
    ),
    child_tv_hours_we = bp50_item(
      "Child self-report (age 10+): hours watching TV on a typical weekend day; replaced by separate Saturday and Sunday items from 1998",
      "1990" = "C0941600",
      "1992" = "C1136400",
      "1994" = "C1365500",
      "1996" = "C1587000"
    ),
    child_tv_hours_sat = bp50_item(
      "Child self-report (age 10+): hours watching TV on a typical Saturday",
      "1998" = "C1925800",
      "2000" = "C2464100",
      "2002" = "C2765500",
      "2004" = "C3045100",
      "2006" = "C3365700",
      "2008" = "C3869400",
      "2010" = "C5117600",
      "2012" = "C5694900",
      "2014" = "C5966800"
    ),
    child_tv_hours_sun = bp50_item(
      "Child self-report (age 10+): hours watching TV on a typical Sunday",
      "1998" = "C1925900",
      "2000" = "C2464200",
      "2002" = "C2765600",
      "2004" = "C3045200",
      "2006" = "C3365900",
      "2008" = "C3869600",
      "2010" = "C5117800",
      "2012" = "C5695100",
      "2014" = "C5967000"
    ),
    child_game_hours_wd = bp50_item(
      "Child self-report (age 10+): hours playing video games on a typical weekday",
      "2006" = "C3365600",
      "2008" = "C3869300",
      "2010" = "C5117500",
      "2012" = "C5694800",
      "2014" = "C5966700"
    ),
    child_game_hours_sat = bp50_item(
      "Child self-report (age 10+): hours playing video games on a typical Saturday",
      "2006" = "C3365800",
      "2008" = "C3869500",
      "2010" = "C5117700",
      "2012" = "C5695000",
      "2014" = "C5966900"
    ),
    child_game_hours_sun = bp50_item(
      "Child self-report (age 10+): hours playing video games on a typical Sunday",
      "2006" = "C3366000",
      "2008" = "C3869700",
      "2010" = "C5117900",
      "2012" = "C5695200",
      "2014" = "C5967100"
    ),
    child_tv_hours_week_band = bp50_item(
      "Child self-report (ages 14-15): banded hours watching TV per week",
      "2012" = "C5695400",
      "2014" = "C5967300"
    ),
    child_game_hours_week_band = bp50_item(
      "Child self-report (ages 14-15): banded hours playing video games per week",
      "2012" = "C5695500",
      "2014" = "C5967400"
    ),
    child_comp_home_owned = bp50_item(
      "Child self-report (age 10+): family owns a home computer",
      "1994" = "C1374200",
      "1996" = "C1596500",
      "1998" = "C1938500",
      "2000" = "C2478400",
      "2002" = "C2774600"
    ),
    child_comp_ever_uses = bp50_item(
      "Child self-report (age 10+): ever uses a computer",
      "2004" = "C3053500",
      "2006" = "C3373900",
      "2008" = "C3877600",
      "2010" = "C5125800",
      "2012" = "C5703300",
      "2014" = "C5975200"
    ),
    child_comp_uses_at_home = bp50_item(
      "Child self-report (age 10+): uses a computer at home",
      "2002" = "C2774700",
      "2004" = "C3053600",
      "2006" = "C3374000",
      "2008" = "C3877700",
      "2010" = "C5125900",
      "2012" = "C5703400",
      "2014" = "C5975300"
    ),
    child_comp_school_freq = bp50_item(
      "Child self-report (age 10+): how often uses a computer at school",
      "1998" = "C1938700",
      "2000" = "C2478700",
      "2002" = "C2774900",
      "2004" = "C3053800",
      "2006" = "C3374200",
      "2008" = "C3877900",
      "2010" = "C5126100",
      "2012" = "C5704000",
      "2014" = "C5975900"
    ),
    child_comp_games_freq_reversed = bp50_item(
      "Child self-report (age 10+): how often uses a computer to play games, on 1994's reversed 1-5 coding (1 almost every day to 5 never)",
      "1994" = "C1375800"
    ),
    child_comp_games_freq = bp50_item(
      "Child self-report (age 10+): how often uses a computer to play games, on the 0-4 coding used from 1996 (0 never to 4 almost every day)",
      "1996" = "C1598100",
      "1998" = "C1940100",
      "2000" = "C2480300",
      "2002" = "C2776200",
      "2004" = "C3054400",
      "2006" = "C3374404",
      "2008" = "C3878104",
      "2010" = "C5126304",
      "2012" = "C5704205",
      "2014" = "C5976105"
    ),
    child_comp_chat_freq = bp50_item(
      "Child self-report (age 10+): how often uses a computer to chat online",
      "2002" = "C2775900",
      "2004" = "C3054200",
      "2006" = "C3374402",
      "2008" = "C3878102",
      "2010" = "C5126302"
    ),
    child_comp_email_im_freq = bp50_item(
      "Child self-report (age 10+): how often uses a computer or mobile device for email, instant messaging or online chat",
      "2012" = "C5704301",
      "2014" = "C5976201"
    ),
    child_comp_hours_week_workschool = bp50_item(
      "Child self-report (age 14+): hours using a computer or mobile device for work or school in a typical week",
      "2012" = "C5703600",
      "2014" = "C5975500"
    ),
    child_comp_hours_week_leisure = bp50_item(
      "Child self-report (age 14+): hours using a computer or mobile device for leisure in a typical week",
      "2012" = "C5703700",
      "2014" = "C5975600"
    ),
    child_parents_limit_tv_freq = bp50_item(
      "Child self-report (age 10+): how often parents limit TV or video game time",
      "1996" = "C1537400",
      "1998" = "C1783200",
      "2000" = "C2293400",
      "2002" = "C2572000",
      "2004" = "C2842700",
      "2006" = "C3346700",
      "2008" = "C3850400",
      "2010" = "C5098800",
      "2012" = "C5676100",
      "2014" = "C5948000"
    ),
    ya_tv_hours_week_band = bp50_item(
      "Young adult self-report: banded hours watching TV in a typical week",
      "2000" = "Y1166700",
      "2002" = "Y1405800",
      "2004" = "Y1657000",
      "2006" = "Y1929800",
      "2008" = "Y2246700",
      "2010" = "Y2599300",
      "2012" = "Y2949600",
      "2014" = "Y3318200",
      "2016" = "Y3662000"
    ),
    ya_game_hours_week_band = bp50_item(
      "Young adult self-report: banded hours playing video games in a typical week",
      "2006" = "Y1930000",
      "2008" = "Y2246900",
      "2010" = "Y2599500",
      "2012" = "Y2949800",
      "2014" = "Y3318400",
      "2016" = "Y3662200"
    ),
    ya_comp_hours_week_band = bp50_item(
      "Young adult self-report: banded total hours using a computer in a typical week",
      "2004" = "Y1633000",
      "2006" = "Y1887100"
    ),
    ya_comp_hours_week_workschool = bp50_item(
      "Young adult self-report: hours using a computer or mobile device for work or school in a typical week",
      "2008" = "Y2201400",
      "2010" = "Y2536600",
      "2012" = "Y2884700",
      "2014" = "Y3255500",
      "2016" = "Y3595400"
    ),
    ya_comp_hours_week_leisure = bp50_item(
      "Young adult self-report: hours using a computer or mobile device for leisure in a typical week",
      "2008" = "Y2201500",
      "2010" = "Y2536700",
      "2012" = "Y2884800",
      "2014" = "Y3255600",
      "2016" = "Y3595500"
    ),
    ya_comp_home_owned = bp50_item(
      "Young adult self-report: owns a home computer",
      "1994" = "Y0361800",
      "1996" = "Y0655800",
      "1998" = "Y0951900"
    ),
    ya_comp_access = bp50_item(
      "Young adult self-report: has access to a computer",
      "2000" = "Y1166800",
      "2002" = "Y1405900",
      "2004" = "Y1657200",
      "2006" = "Y1930100",
      "2008" = "Y2247000",
      "2010" = "Y2599600"
    ),
    ya_comp_school_freq = bp50_item(
      "Young adult self-report: how often uses a computer for school",
      "2000" = "Y1167100",
      "2002" = "Y1406200",
      "2004" = "Y1657500",
      "2006" = "Y1930400"
    ),
    ya_comp_work_freq = bp50_item(
      "Young adult self-report: how often uses a computer for work",
      "2000" = "Y1167400",
      "2002" = "Y1406500",
      "2004" = "Y1657800",
      "2006" = "Y1930700"
    ),
    ya_comp_games_freq = bp50_item(
      "Young adult self-report: how often uses a computer to play games",
      "1994" = "Y0363600",
      "1996" = "Y0657600",
      "1998" = "Y0953400",
      "2000" = "Y1167800",
      "2002" = "Y1406900",
      "2004" = "Y1658200",
      "2006" = "Y1931100"
    ),
    ya_comp_email_freq = bp50_item(
      "Young adult self-report: how often uses a computer for email",
      "1994" = "Y0363900",
      "1996" = "Y0657900",
      "1998" = "Y0953700",
      "2000" = "Y1167500",
      "2002" = "Y1406600",
      "2004" = "Y1657900",
      "2006" = "Y1930800"
    ),
    ya_comp_internet_freq = bp50_item(
      "Young adult self-report: how often uses a computer to reach the internet or other networks",
      "1994" = "Y0364000",
      "1996" = "Y0658000",
      "1998" = "Y0953800",
      "2000" = "Y1167600",
      "2002" = "Y1406700",
      "2004" = "Y1658000",
      "2006" = "Y1930900"
    ),
    ya_parents_limit_tv_freq = bp50_item(
      "Young adult self-report: how often parents limit TV or video game time",
      "1996" = "Y0423400",
      "1998" = "Y0713200",
      "2000" = "Y1019500",
      "2002" = "Y1261900",
      "2004" = "Y1495500",
      "2006" = "Y1745300",
      "2008" = "Y2026100",
      "2010" = "Y2351200",
      "2012" = "Y2680700",
      "2014" = "Y3036100",
      "2016" = "Y3401800"
    ),
    child_tv_rules = bp50_item(
      "Child self-report (age 10+): any rules at home about watching TV",
      "1990" = "C0934000",
      "1992" = "C1124200",
      "1994" = "C1350000",
      "1996" = "C1571300",
      "1998" = "C1909900",
      "2000" = "C2446500",
      "2002" = "C2751800",
      "2004" = "C3031300",
      "2006" = "C3356600",
      "2008" = "C3860300",
      "2010" = "C5108500",
      "2012" = "C5685800",
      "2014" = "C5957700"
    ),
    child_feels_sad_freq = bp50_item(
      "Child self-report (age 10+): how often feels sad and blue",
      "1994" = "C1359100"
    ),
    child_feels_nervous_freq = bp50_item(
      "Child self-report (age 10+): how often feels nervous, tense or on edge",
      "1994" = "C1359200"
    ),
    child_feels_happy_freq = bp50_item(
      "Child self-report (age 10+): how often feels happy",
      "1994" = "C1359300"
    ),
    child_feels_bored_freq = bp50_item(
      "Child self-report (age 10+): how often feels bored",
      "1994" = "C1359400"
    ),
    child_feels_lonely_freq = bp50_item(
      "Child self-report (age 10+): how often feels lonely",
      "1994" = "C1359500"
    ),
    child_feels_tired_freq = bp50_item(
      "Child self-report (age 10+): how often feels tired or worn out",
      "1994" = "C1359600"
    ),
    child_feels_excited_freq = bp50_item(
      "Child self-report (age 10+): how often feels excited about something",
      "1994" = "C1359700"
    ),
    child_feels_too_busy_freq = bp50_item(
      "Child self-report (age 10+): how often feels too busy to get everything done",
      "1994" = "C1359800"
    ),
    child_feels_pressured_freq = bp50_item(
      "Child self-report (age 10+): how often feels pressured by mom or dad",
      "1994" = "C1359900"
    ),
    piat_child_grade = bp50_item(
      "PIAT Mathematics face sheet: child's grade in school, which sets the start item (1990: 0 = kindergarten or less)",
      "1990" = "C0842300",
      "1992" = "C1031600",
      "1994" = "C1250400",
      "1996" = "C1662300",
      "1998" = "C1809500",
      "2000" = "C2321200",
      "2002" = "C2589000"
    ),
    ppvt_raw = bp50_item(
      "PPVT: final score (ceiling minus number of errors) recorded by the interviewer",
      "1992" = "C1113300"
    ),
    ya_felt_depressed = bp50_item(
      "Young adult self-report: CES-D item, felt depressed in the past week",
      "1994" = "Y0336200",
      "1996" = "Y0636600",
      "1998" = "Y0931100",
      "2000" = "Y1161700",
      "2002" = "Y1396800",
      "2004" = "Y1648300",
      "2006" = "Y1920100",
      "2008" = "Y2235300",
      "2010" = "Y2587000",
      "2012" = "Y2935000",
      "2014" = "Y3305100",
      "2016" = "Y3648100"
    )
  )
}

#' Source reference numbers for every stem measured in one survey year
bp50_year_sources <- function(year, dictionary, items, scores) {
  key <- as.character(year)
  from_items <- lapply(items, function(entry) entry$years[[key]])

  rnums <- dictionary$vars$rnum[match(
    paste0(scores, year, "_", year),
    dictionary$vars$qname
  )]
  from_scores <- stats::setNames(
    lapply(rnums, function(rnum) if (is.na(rnum)) NULL else rnum),
    scores
  )

  Filter(Negate(is.null), c(from_items, from_scores))
}

#' Build one survey year's slice of the tidied table
bp50_year_frame <- function(raw, fixed, year, sources, markers, decimals) {
  values <- tibble::as_tibble(lapply(
    sources,
    function(rnums) bp50_take(raw, rnums, decimals)
  ))

  observed <- Reduce(
    `|`,
    lapply(values[intersect(markers, names(values))], function(x) !is.na(x))
  )

  out <- dplyr::bind_cols(
    fixed,
    tibble::tibble(wave = as.character(year)),
    values
  )
  out[observed, , drop = FALSE]
}

#' One stem's values in a survey year, with any implied decimals restored
bp50_take <- function(raw, rnums, decimals) {
  # Two reference numbers are the 2000 HOME B split forms, mutually exclusive.
  Reduce(
    dplyr::coalesce,
    lapply(rnums, function(rnum) {
      bp50_zap(raw[[rnum]]) / 10^bp50_decimals(decimals, rnum)
    })
  )
}

#' Implied decimal places declared for one reference number, zero if none
bp50_decimals <- function(decimals, rnum) {
  if (rnum %in% names(decimals)) decimals[[rnum]] else 0L
}

#' Implied decimal places the codebook declares, by reference number
bp50_implied_decimals <- function(codebook) {
  lines <- readLines(
    grep("\\.cdb$", unlist(codebook), value = TRUE),
    warn = FALSE
  )

  entries <- grep("^[A-Z][0-9]{5}\\.[0-9]{2} +\\[", lines)
  # Notes read "1 IMPLIED DECIMAL" or "(ONE DECIMAL PLACE IMPLIED.)".
  notes <- grep("implied +decimal|decimal +place", lines, ignore.case = TRUE)
  counts <- c(
    "1" = 1L,
    "one" = 1L,
    "2" = 2L,
    "two" = 2L,
    "3" = 3L,
    "three" = 3L
  )
  places <- counts[tolower(sub(
    "^.*?([0-9]+|one|two|three) +(implied +decimal|decimal +place).*$",
    "\\1",
    lines[notes],
    ignore.case = TRUE
  ))]

  if (anyNA(places)) {
    stop(
      paste0(
        "BPIPD-50: unrecognised implied-decimal note in the codebook: ",
        paste(unique(trimws(lines[notes][is.na(places)])), collapse = " | "),
        "."
      ),
      call. = FALSE
    )
  }

  owner <- sub(
    "^([A-Z][0-9]{5})\\.([0-9]{2}).*$",
    "\\1\\2",
    lines[entries][findInterval(notes, entries)]
  )
  declared <- stats::setNames(places, owner)[!duplicated(owner)]
  c(
    declared[setdiff(names(declared), names(bp50_undeclared_decimals()))],
    bp50_undeclared_decimals()
  )
}

#' Implied decimals shown only in the codebook's category listing
bp50_undeclared_decimals <- function() {
  # The 2016 computer-hours listing reads "100 TO 299: 1 to 2.99 hours".
  # BPEXTR 1994 declares one decimal, but its listing runs 0-38 (mean 7.95).
  c(Y3595400 = 2L, Y3595500 = 2L, C1505600 = 0L)
}

#' Set the release-wide missing codes (-1, -2, -3, -7) to NA
bp50_zap <- function(x) {
  x[x %in% c(-1, -2, -3, -7)] <- NA_real_
  x
}

#' Question names, variable and value labels parsed from the loader script
bp50_dictionary <- function(docs) {
  script <- readLines(
    grep("biggerpicture\\.R$", docs, value = TRUE),
    warn = FALSE
  )

  vars <- tibble::tibble(
    rnum = bp50_script_strings(
      script,
      "^names\\(new_data\\) <- c\\(",
      "^# Handle missing values"
    ),
    label = bp50_script_strings(
      script,
      "^varlabels <- c\\(",
      "^qnames = function"
    ),
    qname = bp50_script_strings(
      script,
      "^qnames = function",
      "^return\\(data\\)"
    )
  )

  list(vars = vars, value_labels = bp50_script_value_labels(script))
}

#' The quoted entries of one vector literal in the loader script
bp50_script_strings <- function(script, from, to) {
  start <- grep(from, script)[1]
  end <- grep(to, script)
  lines <- script[start:(end[end > start][1] - 1L)]
  quoted <- regmatches(
    lines,
    regexpr("(\"[^\"]*\"|'[^']*')", lines)
  )
  substr(quoted, 2L, nchar(quoted) - 1L)
}

#' Value labels the loader script attaches with `factor()`, by reference number
bp50_script_value_labels <- function(script) {
  block <- script[
    grep("^vallabels = function", script)[1]:(grep(
      "^vallabels_continuous = function",
      script
    )[1] -
      1L)
  ]
  starts <- grep("^\\s*data\\$[A-Z][0-9]+ <- factor\\(", block)
  ends <- c(starts[-1] - 1L, length(block))

  labels <- Map(
    function(a, b) bp50_parse_value_labels(paste(block[a:b], collapse = "")),
    starts,
    ends
  )
  names(labels) <- sub(
    "^\\s*data\\$([A-Z][0-9]+) <- factor\\(.*$",
    "\\1",
    block[starts]
  )
  Filter(Negate(is.null), labels)
}

#' One `factor()` call's levels and labels, as a named code vector
bp50_parse_value_labels <- function(entry) {
  codes <- suppressWarnings(as.numeric(strsplit(
    sub("^.*levels=c\\(([^)]*)\\).*$", "\\1", entry),
    ","
  )[[1]]))
  labels <- regmatches(entry, gregexpr("\"[^\"]*\"", entry))[[1]]
  labels <- substr(labels, 2L, nchar(labels) - 1L)

  # The loader enumerates the observed values of some continuous scores as
  # their own labels; those carry nothing the code does not already say.
  numeric_labels <- all(!is.na(suppressWarnings(as.numeric(labels))))
  if (anyNA(codes) || length(codes) != length(labels) || numeric_labels) {
    return(NULL)
  }
  stats::setNames(codes, labels)
}

#' Re-attach variable and value labels after the years have been stacked
bp50_apply_labels <- function(
  tidied,
  dictionary,
  constants,
  items,
  scores,
  decimals
) {
  attr(tidied$wave, "label") <- "NLSY79 Child/Young Adult survey year"

  for (stem in names(constants)) {
    tidied[[stem]] <- bp50_label(
      tidied[[stem]],
      bp50_variable_label(dictionary, constants[[stem]]),
      dictionary$value_labels[[constants[[stem]]]]
    )
  }

  for (stem in names(items)) {
    rnums <- unlist(items[[stem]]$years, use.names = FALSE)
    tidied[[stem]] <- bp50_label(
      tidied[[stem]],
      items[[stem]]$label,
      bp50_recent_value_labels(dictionary, rnums, decimals)
    )
  }

  for (stem in scores) {
    rnums <- dictionary$vars$rnum[
      sub("[0-9]{4}_[0-9]{4}$", "", dictionary$vars$qname) == stem
    ]
    tidied[[stem]] <- bp50_label(
      tidied[[stem]],
      bp50_variable_label(dictionary, rnums[1]),
      bp50_recent_value_labels(dictionary, rnums, decimals)
    )
  }

  tidied
}

#' The extract's variable label for one reference number, without its year tag
bp50_variable_label <- function(dictionary, rnum) {
  label <- dictionary$vars$label[match(rnum, dictionary$vars$rnum)]
  trimws(sub("\\s*-?\\s*((19|20)?[0-9]{2})\\s*$", "", label))
}

#' Value labels for a stem, taken from its most recent labelled source year
bp50_recent_value_labels <- function(dictionary, rnums, decimals) {
  # Rounds reword labels without moving codes, so the latest labels serve all.
  coded <- Filter(function(rnum) bp50_decimals(decimals, rnum) == 0L, rnums)
  labels <- Filter(
    Negate(is.null),
    dictionary$value_labels[intersect(coded, names(dictionary$value_labels))]
  )
  if (length(labels) == 0L) {
    return(NULL)
  }
  labels[[length(labels)]]
}

#' Attach a variable label, and value labels where the stem has them
bp50_label <- function(x, label, value_labels) {
  if (is.null(value_labels)) {
    attr(x, "label") <- label
    return(x)
  }
  haven::labelled(x, labels = value_labels, label = label)
}
