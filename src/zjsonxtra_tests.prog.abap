*&---------------------------------------------------------------------*
*& Report zjsonxtra_tests
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjsonxtra_tests.

CLASS lcl_r3tr_xtra DEFINITION.
  PUBLIC SECTION.

    "! Create a transformation object (R3TR XTRA). Call must be followed by COMMIT WORK.
    "! @parameter i_xsltname | Name of transformation object
    "! @parameter i_xslt_source | Can be in Simple Transformation or XSLT.
    CLASS-METHODS create_update_r3tr_xtra_object
      IMPORTING
        i_xsltname           TYPE clike
        VALUE(i_xslt_source) TYPE string.

ENDCLASS.

CLASS lcl_r3tr_xtra IMPLEMENTATION.

  METHOD create_update_r3tr_xtra_object.
    DATA: lo_xslt   TYPE REF TO cl_o2_api_xsltdesc,
          ls_attr   TYPE o2xsltattr,
          lt_source TYPE o2pageline_table.

    ls_attr-xsltdesc = i_xsltname.
    IF cl_o2_api_xsltdesc=>exists( p_xslt_desc = ls_attr-xsltdesc ) = '1'.
      cl_o2_api_xsltdesc=>load(
        EXPORTING
          p_xslt_desc = ls_attr-xsltdesc
        IMPORTING
          p_obj       = lo_xslt
        EXCEPTIONS
          OTHERS      = 1 ).
      lo_xslt->set_changeable(
        EXPORTING
          p_changeable                   = 'X'
        EXCEPTIONS
          action_cancelled               = 1
          error_occured                  = 2
          object_already_changeable      = 3
          object_already_unlocked        = 4
          object_invalid                 = 5
          object_just_created            = 6
          object_locked_by_other_user    = 7
          object_modified                = 8
          permission_failure             = 9
          OTHERS                         = 10 ).
      cl_o2_api_xsltdesc=>prepare_source_table(
        IMPORTING
          e_source_table = lt_source
        CHANGING
          i_string       = i_xslt_source ).
      lo_xslt->set_source(
        EXPORTING
          p_source = lt_source
        EXCEPTIONS
          OTHERS   = 1 ).
    ELSE.
      cl_o2_api_xsltdesc=>create_new_from_string(
        EXPORTING
          p_source = i_xslt_source
          p_attr   = ls_attr
        IMPORTING
          p_obj    = lo_xslt
        EXCEPTIONS
          OTHERS   = 1 ).
    ENDIF.
    lo_xslt->save(
      EXPORTING
        i_source_state         = cl_o2_api_xsltdesc=>c_report_state_active
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                 = 1 ).
    lo_xslt->activate(
      EXCEPTIONS
        OTHERS = 1 ).
    lo_xslt->generate(
      EXCEPTIONS
        OTHERS = 1 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_deserialize_id DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA xsdany TYPE xsdany.
    DATA json_not_asjson TYPE string.
    DATA asjson TYPE string.
    DATA xml_not_json_xml TYPE string.
    DATA json_xml TYPE string.
    DATA lx TYPE REF TO cx_root.
    TYPES: BEGIN OF ty_flight,
             carrid TYPE string,
             connid TYPE string,
           END OF ty_flight,
           ty_flights TYPE STANDARD TABLE OF ty_flight WITH EMPTY KEY.
    DATA flights TYPE ty_flights.

    METHODS json_xsdany FOR TESTING.
    METHODS json_xsdany_empty_member_key FOR TESTING.
    METHODS not_asjson_jsonxml FOR TESTING.
    METHODS asjson_to_simple FOR TESTING.
    METHODS asjson_to_structur FOR TESTING.
    METHODS asjson_to_itab FOR TESTING.

ENDCLASS.

CLASS ltc_deserialize_z_transfo DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA lx TYPE REF TO cx_root.
    TYPES: BEGIN OF ty_flight,
             carrid TYPE string,
             connid TYPE string,
           END OF ty_flight,
           ty_flights TYPE STANDARD TABLE OF ty_flight WITH EMPTY KEY.
    DATA flights TYPE ty_flights.

    METHODS json_to_itab FOR TESTING.

ENDCLASS.

CLASS ltc_json_to_types DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA lx TYPE REF TO cx_root.

    METHODS array_num FOR TESTING.
    METHODS array_object FOR TESTING.
    METHODS json_to_types FOR TESTING.

ENDCLASS.




CLASS ltc_deserialize_id IMPLEMENTATION.

  METHOD json_xsdany.

    json_not_asjson = `{"ROOT":[{"CONNID":"0017","null":null,"emptystring":"","xml":null," spaces ":null,"/":null,"very_long_name_very_long_name_very_long_name":null}]}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML json_not_asjson RESULT root = xsdany.
      CATCH cx_root INTO lx.
    ENDTRY.
    xml_not_json_xml = cl_abap_codepage=>convert_from( xsdany ).
    " see how invalid XML is generated !? (undocumented feature)
    cl_abap_unit_assert=>assert_equals( act = xml_not_json_xml
        exp = `<array><object><CONNID>0017</CONNID><null/><emptystring></emptystring><xml/>< spaces /><//><very_long_name_very_long_name_very_long_name/></object></array>` ).

  ENDMETHOD.


  METHOD json_xsdany_empty_member_key.

    json_not_asjson = `{"ROOT":{"before":null,"":null,"after":null}]}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML json_not_asjson RESULT root = xsdany.
      CATCH cx_root INTO lx.
    ENDTRY.
    xml_not_json_xml = cl_abap_codepage=>convert_from( xsdany ).
    " see how invalid XML is generated !? (undocumented feature)
    cl_abap_unit_assert=>assert_equals( act = xml_not_json_xml
        exp = `<object><before/>` ).

  ENDMETHOD.


  METHOD not_asjson_jsonxml.

    json_not_asjson = `{"ROOT":[{"carrid":"AA","CONNID":"0017"}]}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML json_not_asjson RESULT XML json_xml OPTIONS xml_header = 'no'.
      CATCH cx_root INTO lx.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = json_xml exp = `﻿<object><array name="ROOT"><object><str name="carrid">AA</str><str name="CONNID">0017</str></object></array></object>` ).

  ENDMETHOD.


  METHOD asjson_to_simple.

    DATA carrid TYPE sflight-carrid.
    asjson = `{"ROOT":"AA"}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML asjson RESULT root = carrid.
      CATCH cx_root INTO lx.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = carrid exp = 'AA' ).

  ENDMETHOD.


  METHOD asjson_to_structur.

    DATA flight TYPE sflight.
    asjson = `{"ROOT":{"CARRID":"AA","CONNID":"0017"}}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML asjson RESULT root = flight.
      CATCH cx_root INTO lx.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = flight exp = VALUE sflight( carrid = 'AA' connid = '0017' ) ).

  ENDMETHOD.


  METHOD asjson_to_itab.

    asjson = `{"ROOT":[{"CARRID":"AA","CONNID":"0017"}]}`.
    TRY.
        CALL TRANSFORMATION id SOURCE XML asjson RESULT root = flights.
      CATCH cx_root INTO lx.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = flights exp = VALUE ty_flights( ( carrid = 'AA' connid = '0017' ) ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_deserialize_z_transfo IMPLEMENTATION.

  METHOD json_to_itab.

    CLEAR flights.
    DATA(json) = `[{"carrid":"AA","CONNID":"0017"}]`.
    lcl_r3tr_xtra=>create_update_r3tr_xtra_object(
      EXPORTING
        i_xsltname    = 'ZJSONXTRA_TEST'
        i_xslt_source = |<?sap.transform simple?>\n| &
                        |<tt:transform xmlns:tt="http://www.sap.com/transformation-templates" xmlns:ddic="http://www.sap.com/abapxml/types/dictionary">\n| &
                        |  <tt:root name="ABAPROOT"/>                         \n| &
                        |  <tt:template>                                      \n| &
                        |    <array>                                          \n| &
                        |      <tt:loop ref="ABAPROOT">                       \n| &
                        |        <object>                                     \n| &
                        |           <str name="carrid" tt:value-ref="CARRID"/>\n| &
                        |           <str name="CONNID" tt:value-ref="CONNID"/>\n| &
                        |        </object>                                    \n| &
                        |      </tt:loop>                                     \n| &
                        |    </array>                                         \n| &
                        |  </tt:template>                                     \n| &
                        |</tt:transform>                                      | ).
    COMMIT WORK AND WAIT.
    TRY.
        CALL TRANSFORMATION ('ZJSONXTRA_TEST') SOURCE XML json RESULT abaproot = flights.
      CATCH cx_root INTO lx.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = flights exp = VALUE ty_flights( ( carrid = 'AA' connid = '0017' ) ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_json_to_types IMPLEMENTATION.

  METHOD array_num.
    DATA(json) = `[1,2]`.
    TRY.
        DATA(abap_source_code) = ``.
        CALL TRANSFORMATION id SOURCE XML json RESULT XML DATA(json_xml).
        CALL TRANSFORMATION zjsonxtra_json_to_types SOURCE XML json RESULT XML abap_source_code.
      CATCH cx_root INTO lx.
    ENDTRY.
    SPLIT abap_source_code AT |\r\n| INTO TABLE DATA(abap_source_code_table).
    cl_abap_unit_assert=>assert_equals( act = abap_source_code_table exp = VALUE string_table(
        ( `TYPES ty_main TYPE decfloat34.` )
        ( `TYPES tt_main TYPE STANDARD TABLE OF ty_main WITH EMPTY KEY.` )
        ( `DATA main_json TYPE tt_main.` ) ) ).

  ENDMETHOD.

  METHOD array_object.
    DATA(json) = `[{"a":1}]`.
    TRY.
        DATA(abap_source_code) = ``.
        CALL TRANSFORMATION id SOURCE XML json RESULT XML DATA(json_xml).
        CALL TRANSFORMATION zjsonxtra_json_to_types SOURCE XML json RESULT XML abap_source_code.
      CATCH cx_root INTO lx.
    ENDTRY.
    SPLIT abap_source_code AT |\r\n| INTO TABLE DATA(abap_source_code_table).
    cl_abap_unit_assert=>assert_equals( act = abap_source_code_table exp = VALUE string_table(
        ( `TYPES BEGIN OF ty_main.` )
        ( `TYPES ty_a TYPE decfloat34.` )
        ( `TYPES END OF ty_main.` )
        ( `TYPES tt_main TYPE STANDARD TABLE OF ty_main WITH EMPTY KEY.` )
        ( `DATA main_json TYPE tt_main.` ) ) ).

  ENDMETHOD.

  METHOD json_to_types.
    DATA(json) = `{` && |\r\n|  &&
                 `    "id": "xxxxxxxxxxxxxxxxxxxxxxxxx",` && |\r\n|  &&
                 `    "name": "xxxxxxxxxx",` && |\r\n|  &&
                 `    "description": null,` && |\r\n|  &&
                 `    "imagePullSecret": {` && |\r\n|  &&
                 `        "name": "sap"` && |\r\n|  &&
                 `    },` && |\r\n|  &&
                 `    "requests": {` && |\r\n|  &&
                 `        "cpu": null,` && |\r\n|  &&
                 `        "memory": null,` && |\r\n|  &&
                 `        "abapSystems": 3,` && |\r\n|  &&
                 `        "abapSystemBackups": null` && |\r\n|  &&
                 `    },` && |\r\n|  &&
                 `    "status": {` && |\r\n|  &&
                 `        "conditions": [` && |\r\n|  &&
                 `            {` && |\r\n|  &&
                 `                "ansibleResult": {` && |\r\n|  &&
                 `                    "changed": 0.0,` && |\r\n|  &&
                 `                    "completion": "date",` && |\r\n|  &&
                 `                    "failures": 0.0,` && |\r\n|  &&
                 `                    "ok": 8.0,` && |\r\n|  &&
                 `                    "skipped": 1.0` && |\r\n|  &&
                 `                },` && |\r\n|  &&
                 `                "lastTransitionTime": "date",` && |\r\n|  &&
                 `                "message": "Awaiting next reconciliation",` && |\r\n|  &&
                 `                "reason": "Successful",` && |\r\n|  &&
                 `                "status": "True",` && |\r\n|  &&
                 `                "type": "Running"` && |\r\n|  &&
                 `            }` && |\r\n|  &&
                 `        ],` && |\r\n|  &&
                 `        "resourceQuota": {` && |\r\n|  &&
                 `            "hard": {` && |\r\n|  &&
                 `                "count/abapsystems.abapops.sap": "3",` && |\r\n|  &&
                 `                "cpu": "3",` && |\r\n|  &&
                 `                "memory": "360Gi"` && |\r\n|  &&
                 `            },` && |\r\n|  &&
                 `            "used": {` && |\r\n|  &&
                 `                "count/abapsystems.abapops.sap": "0",` && |\r\n|  &&
                 `                "cpu": "0",` && |\r\n|  &&
                 `                "memory": "0"` && |\r\n|  &&
                 `            }` && |\r\n|  &&
                 `        },` && |\r\n|  &&
                 `        "resoureQuota": {` && |\r\n|  &&
                 `            "hard": {` && |\r\n|  &&
                 `                "count/abapsystems.abapops.sap": "3",` && |\r\n|  &&
                 `                "cpu": "3",` && |\r\n|  &&
                 `                "memory": "360Gi"` && |\r\n|  &&
                 `            },` && |\r\n|  &&
                 `            "used": {` && |\r\n|  &&
                 `                "count/abapsystems.abapops.sap": "0",` && |\r\n|  &&
                 `                "cpu": "0",` && |\r\n|  &&
                 `                "memory": "0"` && |\r\n|  &&
                 `            }}}}`.
    TRY.
        DATA(abap_source_code) = ``.
        CALL TRANSFORMATION id SOURCE XML json RESULT XML DATA(json_xml).
        CALL TRANSFORMATION zjsonxtra_json_to_types SOURCE XML json RESULT XML abap_source_code.
      CATCH cx_root INTO lx.
    ENDTRY.

*    TYPES: BEGIN OF ty_main_json,
*      ty_id TYPE string,
*     " JSON: "id":"xxxxxxxxxxxxxxxxxxxxxxxxx"
*      ty_name TYPE string,
*     " JSON: "name":"xxxxxxxxxx"
*      ty_description TYPE string,
*     "null
*      BEGIN OF ty_imagepullsecret,
*      ty_name TYPE string,
*     " JSON: "name":"sap"
*    	  END OF ty_imagepullsecret,
*
*    	  BEGIN OF ty_requests,
*
*    	  ty_cpu TYPE string,
*     "null
*    	  ty_memory TYPE string,
*     "null
*    	  ty_abapsystems TYPE i,
*
*    	  ty_abapsystembackups TYPE string,
*     "null
*    	  END OF ty_requests,
*
*    	  BEGIN OF ty_status,
*
*    	  BEGIN OF ty_main_json,
*
*    	  BEGIN OF ty_ansibleresult,
*
*    	  ty_changed TYPE i,
*
*    	  ty_completion TYPE string,
*     " JSON: "completion":"date"
*    	  ty_failures TYPE i,
*
*    	  ty_ok TYPE i,
*
*    	  ty_skipped TYPE i,
*
*    	  END OF ty_ansibleresult,
*
*    	  ty_lasttransitiontime TYPE string,
*     " JSON: "lastTransitionTime":"date"
*    	  ty_message TYPE string,
*     " JSON: "message":"Awaiting next reconciliation"
*    	  ty_reason TYPE string,
*     " JSON: "reason":"Successful"
*    	  ty_status TYPE string,
*     " JSON: "status":"True"
*    	  ty_type TYPE string,
*     " JSON: "type":"Running"
*    	  END OF ty_main_json,
*
*    	  tt_conditions TYPE STANDARD TABLE OF ty_conditions,
*
*    	  BEGIN OF ty_resourcequota,
*
*    	  BEGIN OF ty_hard,
*
*    	  ty_countabapsystems TYPE string. " JSON: "count/abapsystems.abapops.sap":"3"
*    TYPES ty_cpu TYPE string. " JSON: "cpu":"3"
*    TYPES ty_memory TYPE string. " JSON: "memory":"360Gi"
*    TYPES END OF ty_hard.
*    TYPES BEGIN OF ty_used.
*    TYPES ty_countabapsystems TYPE string. " JSON: "count/abapsystems.abapops.sap":"0"
*    TYPES ty_cpu TYPE string. " JSON: "cpu":"0"
*    TYPES ty_memory TYPE string. " JSON: "memory":"0"
*    TYPES END OF ty_used.
*    TYPES END OF ty_resourcequota.
*    TYPES BEGIN OF ty_resourequota.
*    TYPES BEGIN OF ty_hard.
*    TYPES ty_countabapsystems TYPE string. " JSON: "count/abapsystems.abapops.sap":"3"
*    TYPES ty_cpu TYPE string. " JSON: "cpu":"3"
*    TYPES ty_memory TYPE string. " JSON: "memory":"360Gi"
*    TYPES END OF ty_hard.
*    TYPES BEGIN OF ty_used.
*    TYPES ty_countabapsystems TYPE string. " JSON: "count/abapsystems.abapops.sap":"0"
*    TYPES ty_cpu TYPE string. " JSON: "cpu":"0"
*    TYPES ty_memory TYPE string. " JSON: "memory":"0"
*    TYPES END OF ty_used.
*    TYPES END OF ty_resourequota.
*    TYPES END OF ty_status.
*    TYPES END OF ty_main_json.
*    DATA main_json TYPE ty_main_json.

  ENDMETHOD.

ENDCLASS.
