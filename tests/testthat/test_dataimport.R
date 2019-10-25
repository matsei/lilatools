library("enspect")

# correctly find correct leg formats
test_that("LEG format example is correct",{
  expect_true(check_legformat("BY_24215903-qsim+vhs")$check)
  expect_true(check_legformat("BY_24215903-QVhS.lila")$check)
  expect_true(check_legformat("BY_24215903-qsim+vhs")$check)
  expect_true(check_legformat("BY_24215903-qvhs.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-e01.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-ice.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-e13-201805150000.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM_WHM.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM_WHM-201805100500.lila")$check)
  expect_true(check_legformat("BY_24215903-tzugtsvhs-e13-201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
  expect_true(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
})

# correctly find false leg formats
test_that("LEG format example is false",{
  expect_false(check_legformat("BY_-qvhs.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM WHM.lila")$check)
  expect_false(check_legformat("BY_24215903-tzugtssim+vhs-ee1-201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-tzugtssim+vhs-e1-201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-tzugtssim+vhs–201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM WHM-201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-e13-201805150000–201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-e13-201805150000-LARSIM_WHM-201805100500-.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-000-201805150000-LARSIM_WHM-201805100500-VAR1.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-000.lila")$check)
  expect_false(check_legformat("BY_24215903-qvhs-000-000.lila")$check)
})

# check if testfile has fileheader
test_that("lila file has file meta info", {
  expect_that(get_lila_filemeta("../../data/exp_lilaeinzel_filemeta.lila"),
              is_identical_to(data.frame(sprache="deutsch", gesamtkommentar="Testkommentar der Datei")))
})
test_that("lila file has no file meta info", {
  expect_true(is.na(get_lila_filemeta("../../data/exp_lilaeinzel.lila", encoding = "latin1")))
})

# check if testfile is read correctly
test_that("example eizellila file ist read",{
  expect_that(get_lila_filemeta("../../data/exp_lilaeinzel.lila"),
              is_identical_to(liladata(data=data.frame(id="1-1",
                                                       time=c("09.09.2019 00:00", "09.09.2019 01:00", "09.09.2019 02:00", "09.09.2019 03:00", "09.09.2019 04:00", "09.09.2019 00:50"),
                                                       values=c(0.9, 0.9, 0.9, 0.9, 0.8, 0.8)),
                                       meta=data.frame(id='1-1',
                                                       station="Rebbelroth",
                                                       gewaesser="Agger",
                                                       stationsnummer=01320010,
                                                       stationskennung='REBR',
                                                       datenart='Q',
                                                       dimension='cbm/s',
                                                       datenbezug='GTS',
                                                       zeitintervall='01:00',
                                                       datentyp='M',
                                                       datenursprung='vhs',
                                                       flaeche=109.860,
                                                       flusskilometer=63.693,
                                                       vorhersagezeitpunkt='08.09.2019 13:00',
                                                       kommentar='Abfluss Vorhersage',
                                                       austauschkennung='RP_01320010-qvhs-d01-201908270700')
                                       )))
})

