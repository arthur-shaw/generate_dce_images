source('R/03_create_template.R')
source('R/04_validate_template.R')
source('R/05_template_to_yaml.R')

cat('\n')
cat('═══════════════════════════════════════════════════════════════\n')
cat('COMPREHENSIVE INTEGRATION TESTS\n')
cat('═══════════════════════════════════════════════════════════════\n')

# TEST 1: Generate Morocco template
cat('\n[TEST 1] Generate Morocco template\n')
create_excel_template(
  country = 'ma',
  languages = c('fr', 'ar'),
  labels_path = 'inst/labels.yaml',
  schema_path = 'inst/schema.yaml',
  output_path = 'test_integration_ma.xlsx'
)
stopifnot(file.exists('test_integration_ma.xlsx'))
cat('✓ PASS: Template generated\n')

# TEST 2: Validate freshly-generated template
cat('\n[TEST 2] Validate template\n')
val_result <- validate_excel_template(
  path = 'test_integration_ma.xlsx',
  country = 'ma',
  languages = c('fr', 'ar'),
  schema_path = 'inst/schema.yaml'
)
cat('Validation returned:', nrow(val_result$issues), 'issues\n')
cat('✓ PASS: Validator executed\n')

# TEST 3: Check template structure
cat('\n[TEST 3] Verify template structure\n')
library(readxl)
sheets <- excel_sheets('test_integration_ma.xlsx')
expected_sheets <- c('Instructions', 'tbl_labels', 'cost', 'hours', 'location', 'quality')
stopifnot(identical(sort(sheets), sort(expected_sheets)))
cat('✓ PASS: All expected sheets present\n')

# TEST 4: Check tbl_labels contents
cat('\n[TEST 4] Verify tbl_labels contents\n')
df_labels <- read_xlsx('test_integration_ma.xlsx', sheet = 'tbl_labels')
expected_keys <- c('cost', 'hours', 'location', 'quality', 'attribute', 'option_a', 'option_b')
actual_keys <- df_labels$key
stopifnot(identical(sort(actual_keys), sort(expected_keys)))
cat('✓ PASS: tbl_labels has correct keys\n')

# TEST 5: Check cost contents
cat('\n[TEST 5] Verify cost sheet contents\n')
df_cost <- read_xlsx('test_integration_ma.xlsx', sheet = 'cost')
expected_cost_keys <- c('lvl_0', 'lvl_1', 'lvl_2', 'lvl_3', 'lvl_4', 'lvl_5')
actual_cost_keys <- df_cost$key
stopifnot(identical(actual_cost_keys, expected_cost_keys))
cat('✓ PASS: cost sheet has correct keys\n')

# TEST 6: Check language columns exist
cat('\n[TEST 6] Verify language columns\n')
col_names <- names(df_cost)
stopifnot('key' %in% col_names)
stopifnot('en' %in% col_names)
stopifnot('fr' %in% col_names)
stopifnot('ar' %in% col_names)
cat('✓ PASS: All expected language columns present\n')

# TEST 7: Verify values are pre-filled from YAML
cat('\n[TEST 7] Verify pre-filled values from YAML\n')
df_hours <- read_xlsx('test_integration_ma.xlsx', sheet = 'hours')
# Check that at least some values are non-empty
non_empty_count <- sum(!is.na(df_hours$fr) & df_hours$fr != '')
stopifnot(non_empty_count > 0)
cat(glue::glue('✓ PASS: {non_empty_count} pre-filled values in hours sheet\n'))

# TEST 8: Schema structure test
cat('\n[TEST 8] Verify schema structure\n')
schema <- yaml::read_yaml('inst/schema.yaml')
stopifnot('indexed_by' %in% names(schema$tbl_labels))
stopifnot('keys' %in% names(schema$tbl_labels))
stopifnot('indexed_by' %in% names(schema$tbl_content$cost))
stopifnot(identical(schema$tbl_labels$indexed_by, 'language'))
stopifnot(identical(schema$tbl_content$cost$indexed_by, c('country', 'language')))
cat('✓ PASS: Schema structure correct\n')

# TEST 9: Test with English as a language
cat('\n[TEST 9] Generate Ghana template with English\n')
create_excel_template(
  country = 'gh',
  languages = c('en', 'tw'),
  labels_path = 'inst/labels.yaml',
  schema_path = 'inst/schema.yaml',
  output_path = 'test_integration_gh.xlsx'
)
stopifnot(file.exists('test_integration_gh.xlsx'))
df_gh <- read_xlsx('test_integration_gh.xlsx', sheet = 'tbl_labels')
# Should have key, en, tw (no duplicate en)
stopifnot(length(names(df_gh)) == 3)
stopifnot(identical(names(df_gh), c('key', 'en', 'tw')))
cat('✓ PASS: English language variant works correctly\n')

cat('\n')
cat('═══════════════════════════════════════════════════════════════\n')
cat('✓ ALL INTEGRATION TESTS PASSED\n')
cat('═══════════════════════════════════════════════════════════════\n')
