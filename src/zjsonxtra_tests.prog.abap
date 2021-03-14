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

    METHODS num FOR TESTING.
    METHODS str FOR TESTING.
    METHODS bool FOR TESTING.
    METHODS null FOR TESTING.
    METHODS array_num FOR TESTING.
    METHODS array_object FOR TESTING.
    METHODS object_array FOR TESTING.
    METHODS array_array FOR TESTING.
    METHODS object_object FOR TESTING.
    METHODS empty_array FOR TESTING.
    METHODS empty_object FOR TESTING.
    METHODS object_object_array FOR TESTING.

    TYPES: BEGIN OF ty_jsonxtra_to_types,
             json_xml TYPE string,
             abap     TYPE string_table,
           END OF ty_jsonxtra_to_types.

    METHODS zjsonxtra_json_to_types
      IMPORTING
        json            TYPE string
      RETURNING
        VALUE(r_result) TYPE ty_jsonxtra_to_types.

ENDCLASS.

CLASS ltc_json_to_st DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA lx TYPE REF TO cx_root.

    METHODS num FOR TESTING.
    METHODS str FOR TESTING.
    METHODS bool FOR TESTING.
    METHODS null FOR TESTING.
    METHODS array_num FOR TESTING.
    METHODS object_num FOR TESTING.
    METHODS array_object FOR TESTING.
    METHODS object_array FOR TESTING.
    METHODS array_array FOR TESTING.
    METHODS object_object FOR TESTING.
    METHODS empty_array FOR TESTING.
    METHODS empty_object FOR TESTING.
    METHODS object_object_array FOR TESTING.

    TYPES: BEGIN OF ty_jsonxtra_to_st,
             json_xml TYPE string,
             st       TYPE string_table,
           END OF ty_jsonxtra_to_st.

    METHODS zjsonxtra_json_to_st
      IMPORTING
        json            TYPE string
      RETURNING
        VALUE(r_result) TYPE ty_jsonxtra_to_st.

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
    "<array><object><str name="carrid">AA</str><str name="CONNID">0017</str></object></array>
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

  METHOD num.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `1` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<num>1</num>`
            abap = VALUE #(
                ( `DATA json TYPE decfloat34.` ) ) ) ).
  ENDMETHOD.

  METHOD str.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `"a"` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<str>a</str>`
            abap = VALUE #(
                ( `DATA json TYPE string.` ) ) ) ).
  ENDMETHOD.

  METHOD bool.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `true` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<bool>true</bool>`
            abap = VALUE #(
                ( `DATA json TYPE abap_bool.` ) ) ) ).
  ENDMETHOD.

  METHOD null.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `null` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<null/>`
            abap = VALUE #(
                ( `DATA json TYPE string.` ) ) ) ).
  ENDMETHOD.

  METHOD array_num.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `[1,2]` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<array><num>1</num><num>2</num></array>`
            abap = VALUE #(
                ( `DATA json TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY.` ) ) ) ).
  ENDMETHOD.

  METHOD array_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `[{"a":1}]` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<array><object><num name="a">1</num></object></array>`
            abap = VALUE #(
                ( `TYPES:` )
                ( `BEGIN OF ty1,` )
                ( `a TYPE decfloat34,` )
                ( `END OF ty1.` )
                ( `DATA json TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.` ) ) ) ).
  ENDMETHOD.

  METHOD object_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `{"a":[1]}` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<object><array name="a"><num>1</num></array></object>`
            abap = VALUE #(
                ( `TYPES:` )
                ( `BEGIN OF ty1,` )
                ( `a TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY,` )
                ( `END OF ty1.` )
                ( `DATA json TYPE ty1.` ) ) ) ).
  ENDMETHOD.

  METHOD array_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `[[1]]` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<array><array><num>1</num></array></array>`
            abap = VALUE #(
                ( `TYPES:` )
                ( `ty1 TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY.` )
                ( `DATA json TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.` ) ) ) ).
  ENDMETHOD.

  METHOD object_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `{"a":{"b":1}}` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<object><object name="a"><num name="b">1</num></object></object>`
            abap = VALUE #(
                ( `TYPES:` )
                ( `BEGIN OF ty1,` )
                ( `BEGIN OF a,` )
                ( `b TYPE decfloat34,` )
                ( `END OF a,` )
                ( `END OF ty1.` )
                ( `DATA json TYPE ty1.` ) ) ) ).
  ENDMETHOD.

  METHOD empty_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `{}` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<object/>`
            abap = VALUE #(
                ( `DATA json TYPE string.` ) ) ) ).
  ENDMETHOD.

  METHOD empty_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `[]` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<array/>`
            abap = VALUE #(
                ( `DATA json TYPE STANDARD TABLE OF string WITH EMPTY KEY.` ) ) ) ).
  ENDMETHOD.

  METHOD object_object_array.

    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_types(
            `{"a":{"b":[1]}}` )
        exp = VALUE ty_jsonxtra_to_types(
            json_xml =
                `<object><object name="a"><array name="b"><num>1</num></array></object></object>`
            abap = VALUE #(
                ( `TYPES:` )
                ( `BEGIN OF ty1,` )
                ( `BEGIN OF a,` )
                ( `b TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY,` )
                ( `END OF a,` )
                ( `END OF ty1.` )
                ( `DATA json TYPE ty1.` ) ) ) ).
  ENDMETHOD.

  METHOD zjsonxtra_json_to_types.
    DATA(abap_source_code) = ``.
    CALL TRANSFORMATION id SOURCE XML json RESULT XML DATA(json_xml) OPTIONS xml_header = 'no'.
    r_result-json_xml = cl_abap_codepage=>convert_from( json_xml ).
    CALL TRANSFORMATION zjsonxtra_json_to_types SOURCE XML json RESULT XML abap_source_code.
    FIND ALL OCCURRENCES OF REGEX 'ty_\w+' IN abap_source_code IGNORING CASE RESULTS DATA(matches).
    DATA(typenames) = VALUE string_table(
        FOR GROUPS <group_match> OF <match> IN matches
        GROUP BY substring( val = abap_source_code off = <match>-offset len = <match>-length )
        ( <group_match> ) ).
    LOOP AT typenames ASSIGNING FIELD-SYMBOL(<typename>).
      REPLACE ALL OCCURRENCES OF <typename> IN abap_source_code WITH |ty{ sy-tabix }|.
    ENDLOOP.
    SPLIT abap_source_code AT |\r\n| INTO TABLE r_result-abap.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_json_to_st IMPLEMENTATION.

  METHOD num.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `1` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<num>1</num>`
            st = VALUE #(
                ( `<num tt:value-ref=".ABAPROOT"/>` ) ) ) ).
  ENDMETHOD.

  METHOD str.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `"a"` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<str>a</str>`
            st = VALUE #(
                ( `<str tt:value-ref=".ABAPROOT"/>` ) ) ) ).
  ENDMETHOD.

  METHOD bool.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `true` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<bool>true</bool>`
            st = VALUE #(
                ( `<bool tt:value-ref=".ABAPROOT"/>` ) ) ) ).
  ENDMETHOD.

  METHOD null.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `null` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<null/>`
            st = VALUE #(
                ( `<null tt:value-ref=".ABAPROOT"/>` ) ) ) ).
  ENDMETHOD.

  METHOD array_num.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `[1,2]` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<array><num>1</num><num>2</num></array>`
            st = VALUE #(
                ( `<array><tt:loop ref=".ABAPROOT"><num/></tt:loop></array>` ) ) ) ).
  ENDMETHOD.

  METHOD object_num.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `{"a":1}` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<object><num name="a">1</num></object>`
            st = VALUE #(
                ( `<object ref=".ABAPROOT"><num name="a" tt:value-ref="a"/></object>` ) ) ) ).
  ENDMETHOD.

  METHOD array_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `[{"a":1}]` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<array><object><num name="a">1</num></object></array>`
            st = VALUE #(
                ( `<array><tt:loop ref=".ABAPROOT"><object><num name="a" tt:value-ref="a"/></object></tt:loop></array>` ) ) ) ).
  ENDMETHOD.

  METHOD object_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `{"a":[1]}` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<object><array name="a"><num>1</num></array></object>`
            st = VALUE #(
                ( `<object ref=".ABAPROOT"><array><tt:loop ref="a"><num/></tt:loop></array></object>` ) ) ) ).
  ENDMETHOD.

  METHOD array_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `[[1]]` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<array><array><num>1</num></array></array>`
            st = VALUE #(
                ( `` ) ) ) ).
  ENDMETHOD.

  METHOD object_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `{"a":{"b":1}}` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<object><object name="a"><num name="b">1</num></object></object>`
            st = VALUE #(
                ( `` ) ) ) ).
  ENDMETHOD.

  METHOD empty_object.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `{}` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<object/>`
            st = VALUE #(
                ( `` ) ) ) ).
  ENDMETHOD.

  METHOD empty_array.
    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `[]` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<array/>`
            st = VALUE #(
                ( `` ) ) ) ).
  ENDMETHOD.

  METHOD object_object_array.

    cl_abap_unit_assert=>assert_equals(
        act = zjsonxtra_json_to_st(
            `{"a":{"b":[1]}}` )
        exp = VALUE ty_jsonxtra_to_st(
            json_xml =
                `<object><object name="a"><array name="b"><num>1</num></array></object></object>`
            st = VALUE #(
                ( `` ) ) ) ).
  ENDMETHOD.

  METHOD zjsonxtra_json_to_st.
    DATA(st_source_code) = ``.
    CALL TRANSFORMATION id SOURCE XML json RESULT XML DATA(json_xml) OPTIONS xml_header = 'no'.
    r_result-json_xml = cl_abap_codepage=>convert_from( json_xml ).

    CALL TRANSFORMATION zjsonxtra_json_to_st SOURCE XML json RESULT XML st_source_code.

    DATA(xpp2) = NEW cl_xslt_processor( ).
    xpp2->set_source_string( st_source_code ).
    DATA(result) = ``.
    xpp2->set_expression( expression = '//tt:transform/tt:template/*' nsdeclarations = 'tt http://www.sap.com/transformation-templates' ).

    xpp2->run( ' ' ).

    DATA(nodes) = xpp2->get_nodes( ).
    DATA(ixml) = cl_ixml=>create( ).
    DATA(string) = ``.
    DATA(stream_factory) = ixml->create_stream_factory( ).
    DATA(ostream) = stream_factory->create_ostream_cstring( string = string ).
    DO nodes->get_length( ) TIMES.
      DATA(i) = sy-index.
      DATA(node) = nodes->get_item( i - 1 ).
      string = ``.
      node->render( ostream = ostream recursive = 'X' ).
      APPEND string TO r_result-st.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_temporary DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test2 FOR TESTING.
ENDCLASS.

CLASS ltc_temporary IMPLEMENTATION.
  METHOD test2.
    TYPES:
      BEGIN OF ty1,
        a TYPE decfloat34,
      END OF ty1.
    DATA json TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
    TRY.

        CALL TRANSFORMATION zjsonxtra_test2 SOURCE XML `[{"a":1}]` RESULT abaproot = json.
      CATCH cx_root INTO DATA(lx).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
