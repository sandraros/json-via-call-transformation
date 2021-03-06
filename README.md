# json-via-call-transformation
CALL TRANSFORMATION is the fastest way to serialize and deserialize JSON in ABAP.

The big problem is that it takes time to create a ST transformation (to use with CALL TRANSFORMATION), it's why many people prefer using /UI2/CL_JSON, /UI5/CL_JSON_PARSER, CL_FDT_JSON or CL_TREX_JSON* classes, as long as high performance is not needed.

I'd like to propose here a tool to generate automatically a ST transformation and ABAP code from a JSON.
