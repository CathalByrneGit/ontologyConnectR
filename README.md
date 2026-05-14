<!-- README.md is generated from README.Rmd. Please edit that file -->

# ontologyConnectR

**Live external sources as ontology object types — no materialisation, no
custom interface.**

`ontologyConnectR` backs ontology object types against REST APIs, FHIR R4
servers, JDBC databases, and flat-file sources without copying data into a
local database. Every `dplyr::filter()` + `dplyr::collect()` call translates
in real time to the source's native query language.

## How it works

`ontologyConnectR` implements the **dbplyr backend pattern** — the same
architecture used by `dtplyr`, `sparklyr`, and `multidplyr`. Each live source
is a `LiveConnection` S4 object that extends `DBI::DBIConnection`. dbplyr
generates standard SQL; `ontologyConnectR` intercepts it at `dbGetQuery()`,
parses it, maps property IDs to source field names, and dispatches to the
appropriate backend.

```
dplyr::tbl(live_con, "Patient")   # lazy — no network call yet
  |> dplyr::filter(gender == "female", birth_date > "1990-01-01")
  |> dplyr::collect()             # ← triggers live_execute()
         ↓
   parse SQL → map columns → dispatch
         ↓
   FHIR:  GET /Patient?gender=female&birthdate=gt1990-01-01
   REST:  GET /patients?gender=female&fields=...
   JDBC:  SELECT ... FROM patients WHERE gender = 'female' AND ...
         ↓
   data.frame (columns = property IDs)
```

Because `LiveConnection` is a valid DBI connection, **`objectSetsR` requires
no changes** — `object_set()` returns the same `tbl_dbi` lazy object whether
the source is a Postgres database or a live FHIR server.

## Installation

``` r
remotes::install_github("cathalbyrnegit/ontologyconnectr")
```

## Quick start

### 1. Define a schema map

A schema map connects ontology property IDs to source field names:

``` r
library(ontologyConnectR)
library(dplyr)

patient_schema <- list(
  Patient = list(
    resource = "Patient",           # FHIR resource type
    pk       = "patient_id",
    columns  = list(
      patient_id   = "id",
      family_name  = "name.where(use='official').family",
      given_name   = "name.where(use='official').given.first()",
      birth_date   = "birthDate",
      gender       = "gender"
    )
  )
)
```

### 2. Create a live connection

``` r
fhir_con <- live_fhir(
  schema_map = patient_schema,
  base_url   = "https://hapi.fhir.org/baseR4",
  auth       = conn_auth_bearer(env_var = "FHIR_TOKEN"),  # optional
  cache_ttl  = 300L   # 5-minute result cache
)
```

### 3. Register it in an ontology context

``` r
bundle <- list(
  object_types = list(
    Patient = list(id = "Patient", extensions = list())
  ),
  link_types = list()
)

ctx <- ontology_context(
  bundle           = bundle,
  live_connections = list(Patient = fhir_con)
)
```

### 4. Query with dplyr

``` r
# Filters pushed to FHIR server — no data fetched until collect()
patients <- object_set(ctx, "Patient") |>
  filter(gender == "female") |>
  filter(birth_date > "1990-01-01") |>
  collect()

# Inspect the SQL dbplyr generated (before translation)
object_set(ctx, "Patient") |>
  filter(gender == "female") |>
  show_query()
#> <SQL>
#> SELECT *
#> FROM "Patient"
#> WHERE ("gender" = 'female')
```

## Backends

### REST API — `live_rest()`

General-purpose JSON REST connector. Equality filters are pushed as query
parameters; other operators are applied in R after fetching.

``` r
rest_con <- live_rest(
  schema_map = list(
    Airport = list(
      resource = "airports",
      pk       = "airport_id",
      columns  = list(
        airport_id = "id",
        name       = "name",
        country    = "country_code",
        iata_code  = "iata"
      )
    )
  ),
  base_url   = "https://api.aviationstack.com/v1",
  auth       = conn_auth_api_key("access_key", key_env_var = "AVIATION_KEY"),
  pagination = conn_pagination_offset(page_param = "offset",
                                       size_param = "limit",
                                       page_size  = 100L),
  cache_ttl  = 60L
)
```

### FHIR R4 — `live_fhir()`

Native FHIR search push-down for `=`, `!=`, `>`, `>=`, `<`, `<=`, `IN`, and
`LIKE`. Bundle pagination follows `rel="next"` links automatically. Schema map
values are FHIRPath expressions.

``` r
fhir_con <- live_fhir(
  schema_map = list(
    Encounter = list(
      resource = "Encounter",
      pk       = "encounter_id",
      columns  = list(
        encounter_id = "id",
        status       = "status",
        class_code   = "class.code",
        patient_ref  = "subject.reference",
        start_date   = "period.start"
      )
    )
  ),
  base_url = "https://hapi.fhir.org/baseR4"
)
```

| Filter op | FHIR search parameter | Example |
|---|---|---|
| `==` | exact match | `?family=Smith` |
| `>` | `gt` prefix | `?birthdate=gt1990-01-01` |
| `>=` | `ge` prefix | `?birthdate=ge1990-01-01` |
| `<` | `lt` prefix | `?birthdate=lt2000-01-01` |
| `<=` | `le` prefix | `?birthdate=le2000-01-01` |
| `%in%` | comma-separated | `?status=active,inactive` |
| `grepl()` | `:contains` modifier | `?family:contains=Smi` |

### JDBC — `live_jdbc()`

For databases without native R DBI drivers (SAP, Teradata, some Oracle
setups). The rewritten SQL can optionally be translated to the target dialect
via `sqlglotR::sg_translate()`.

``` r
jdbc_con <- live_jdbc(
  schema_map = list(
    Employee = list(
      resource = "HR.EMPLOYEES",
      pk       = "employee_id",
      columns  = list(
        employee_id = "EMP_ID",
        full_name   = "FULL_NAME",
        department  = "DEPT_CODE",
        hire_date   = "HIRE_DT"
      )
    )
  ),
  driver_class = "com.sap.db.jdbc.Driver",
  jdbc_url     = "jdbc:sap://myhost:30015",
  credentials  = list(username = "dbuser", password_env_var = "SAP_PASSWORD"),
  dialect      = "tsql"
)
```

### ICIJ Offshore Leaks — `live_icij()`

A pre-built connector for the
[ICIJ Offshore Leaks](https://offshoreleaks.icij.org/) database. CSV files
are loaded into an in-memory DuckDB instance. A bundled 20-row sample fixture
is included so tests and vignettes run without any download.

``` r
# Use the bundled sample (no download)
icij_con <- live_icij()

# Or point at the full ICIJ CSV download
# icij_con <- live_icij(csv_dir = "~/data/icij")

ctx <- ontology_context(
  bundle = list(
    object_types = list(
      Entity  = list(id = "Entity",  extensions = list()),
      Officer = list(id = "Officer", extensions = list())
    ),
    link_types = list()
  ),
  live_connections = list(Entity = icij_con, Officer = icij_con)
)

# Panama Papers entities incorporated in the British Virgin Islands
bvi <- object_set(ctx, "Entity") |>
  filter(source_id == "Panama Papers", jurisdiction == "BVI") |>
  collect()
```

## Authentication

``` r
# Bearer token (JWT, OAuth access token)
conn_auth_bearer(env_var = "MY_TOKEN")

# HTTP Basic Auth
conn_auth_basic(username = "alice", password_env_var = "MY_PASSWORD")

# API key in a custom header
conn_auth_api_key(header = "X-API-Key", key_env_var = "MY_API_KEY")

# OAuth2 client credentials — tokens cached and refreshed automatically
conn_auth_oauth2(
  token_url             = "https://auth.example.com/oauth/token",
  client_id             = "my-client",
  client_secret_env_var = "MY_SECRET",
  scope                 = "read:patients"
)
```

## Caching

Set `cache_ttl` (seconds) on any constructor to avoid redundant source calls:

``` r
conn <- live_rest(schema_map, base_url, cache_ttl = 300L)  # 5-minute TTL

conn_invalidate(conn)           # clear now (e.g. after a write)
conn <- conn_set_ttl(conn, 60L) # change TTL at runtime
```

## Schema map format

Each entry in `schema_map` describes one object type:

``` r
list(
  MyType = list(
    resource = "source_table_or_path",  # table / REST path / FHIR resource
    pk       = "property_id_of_pk",
    columns  = list(
      property_id = "source_field_name",
      another_id  = "another_source_col"
    )
  )
)
```

`conn_from_bundle()` constructs a connection from an `ontologySpecR` bundle
extension, so schema maps can be stored alongside bundle definitions and
instantiated at runtime without repeating the mapping in application code.

## Mixed DBI + live connections

One context can hold both DBI-backed and live-backed object types:

``` r
ctx <- ontology_context(
  bundle           = bundle,
  connection       = dbi_con,           # Postgres for most types
  live_connections = list(
    Patient  = fhir_con,                # FHIR for Patient
    AuditLog = rest_con                 # REST API for AuditLog
  )
)
```

Each type routes to the correct backend automatically.

## How `live_execute()` works

When `dplyr::collect()` fires, dbplyr compiles the lazy query to SQL and
calls `DBI::dbGetQuery(live_con, sql)`. `ontologyConnectR` intercepts there:

1. **Parse** — `parse_live_query()` extracts the table, SELECT list, WHERE
   filters, LIMIT, and OFFSET from the SQL string.
2. **Map to source** — `map_to_source()` translates property IDs to source
   field names using the schema map entry for that object type.
3. **Dispatch** — `dispatch_query()` (an S4 generic) routes to the backend
   method, which builds and executes the native query (HTTP, FHIR search,
   JDBC SQL).
4. **Map from source** — `map_from_source()` renames result columns back to
   property IDs before returning.
5. **Cache** — the result is stored in `cachem::cache_mem()` keyed on the SQL
   string, if `cache_ttl > 0`.

## Package structure

```
R/
  live_connection.R   LiveConnection S4 class hierarchy (+ LiveResult)
  dbi_methods.R       DBI interface: dbGetQuery, dbSendQuery, dbFetch, quoting
  execute.R           live_execute(), parse_live_query(), map_to/from_source()
  auth.R              conn_auth_bearer/basic/api_key/oauth2
  pagination.R        conn_pagination_offset/cursor/link
  cache.R             conn_invalidate(), conn_set_ttl()
  filters.R           translate_filters_to_r() (R-side fallback)
  schema_map.R        validate_live_schema_map(), conn_from_bundle()
  context.R           ontology_context(), object_set()
  backends/
    rest.R            LiveRestConnection + dispatch_query method
    fhir.R            LiveFhirConnection + FHIRPath evaluator
    jdbc.R            LiveJdbcConnection + dialect bridge
    icij.R            LiveIcijConnection + DuckDB CSV loader
```

## License

MIT
