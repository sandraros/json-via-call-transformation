# json-via-call-transformation
CALL TRANSFORMATION is the fastest way to serialize and deserialize JSON in ABAP.

The big problem is that it takes time to create a ST transformation (to use with CALL TRANSFORMATION), it's why many people prefer using /UI2/CL_JSON, /UI5/CL_JSON_PARSER, CL_FDT_JSON or CL_TREX_JSON* classes, as long as high performance is not needed.

I'd like to propose here a tool to generate automatically a ST transformation and ABAP code from a JSON.

The XSLT transformation would convert the following JSON-XML into ABAP code (not sure whether it's possible):
```
<num>              TYPES ty_auto1 type decfloat34

<array>            TYPES tt_auto1 type standard table of decfloat34
  <num> 

<array>            TYPES tt_auto1 type standard table of ty_auto1
  <object>         TYPES begin of ty_auto1
    <str name="x"> TYPES x TYPE string

<array>            TYPES tt_auto1 type standard table of tt_auto2
  <array>          TYPES tt_auto2 type standard table of string
    <str>

<object>           TYPES begin of ty_main
  <array name="x"> TYPES x type standard table of string
    <str>

<object>           TYPES begin of ty_auto1
  <array name="x"> TYPES x type standard table of tt_auto2
    <array>        TYPES tt_auto2 type standard table of string
      <str>

<object>           TYPES begin of ty_auto1
  <array name="x"> TYPES x type standard table of ty_auto2
    <object>

<object>            TYPES begin of ty_auto1
  <object name="x"> TYPES begin of x
    <str name="y">  TYPES y type string
```
