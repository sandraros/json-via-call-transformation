*&---------------------------------------------------------------------*
*& Report zjsonxtra_tests
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjsonxtra_tests.

CLASS lcx_r3tr_xtra DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_r3tr_xtra DEFINITION.
  PUBLIC SECTION.

    "! Create a transformation object (R3TR XTRA). Call must be followed by COMMIT WORK.
    "! @parameter i_xsltname | Name of transformation object
    "! @parameter i_xslt_source | Can be in Simple Transformation or XSLT.
    CLASS-METHODS create_update_r3tr_xtra_object
      IMPORTING
        i_xsltname           TYPE clike
        VALUE(i_xslt_source) TYPE string
      RAISING
        lcx_r3tr_xtra.

ENDCLASS.

CLASS lcl_r3tr_xtra IMPLEMENTATION.

  METHOD create_update_r3tr_xtra_object.
    DATA: lo_xslt   TYPE REF TO cl_o2_api_xsltdesc,
          ls_attr   TYPE o2xsltattr,
          lt_source TYPE o2pageline_table.

    DATA(normalized_xslt_source) = replace( val = i_xslt_source sub = |\r\n| with = |\n| occ = 0 ).

    ls_attr-xsltdesc = i_xsltname.
    IF cl_o2_api_xsltdesc=>exists( p_xslt_desc = ls_attr-xsltdesc ) = '1'.
      cl_o2_api_xsltdesc=>load(
        EXPORTING
          p_xslt_desc = ls_attr-xsltdesc
        IMPORTING
          p_obj       = lo_xslt
        EXCEPTIONS
          OTHERS      = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
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
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
      cl_o2_api_xsltdesc=>prepare_source_table(
        IMPORTING
          e_source_table = lt_source
        CHANGING
          i_string       = normalized_xslt_source ).
      lo_xslt->set_source(
        EXPORTING
          p_source = lt_source
        EXCEPTIONS
          OTHERS   = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
    ELSE.
      cl_o2_api_xsltdesc=>create_new_from_string(
        EXPORTING
          p_source = normalized_xslt_source
          p_attr   = ls_attr
        IMPORTING
          p_obj    = lo_xslt
        EXCEPTIONS
          OTHERS   = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
    ENDIF.
    lo_xslt->save(
      EXPORTING
        i_source_state         = cl_o2_api_xsltdesc=>c_report_state_active
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                 = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.
    lo_xslt->activate(
      EXCEPTIONS
        OTHERS = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.
    lo_xslt->generate(
      EXCEPTIONS
        OTHERS = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.

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
      ty1 TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY.
    DATA json TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
    TRY.
*        CALL TRANSFORMATION zjsonxtra_test2 SOURCE XML `[[1]]` RESULT abaproot = json.
      CATCH cx_root INTO DATA(lx).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_ui DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_ui DEFINITION.
  PUBLIC SECTION.
    METHODS pbo.
    METHODS pai
      IMPORTING
        ucomm TYPE sscrfields-ucomm.
    METHODS run
      RAISING
        lcx_r3tr_xtra
        lcx_ui.
    METHODS exit.
  PRIVATE SECTION.
    DATA: textedit TYPE REF TO cl_gui_textedit,
          json     TYPE REF TO string.
ENDCLASS.

CLASS lcl_ui IMPLEMENTATION.

  METHOD pbo.
    LOOP AT SCREEN.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
    IF textedit IS NOT BOUND.
      CREATE OBJECT textedit
        EXPORTING
          parent = cl_gui_container=>screen0.
      DATA(global_json) = |({ sy-repid })JSON|.
      ASSIGN (global_json) TO FIELD-SYMBOL(<json>).
      textedit->set_textstream( <json> ).
      json = REF #( <json> ).
    ENDIF.
  ENDMETHOD.

  METHOD pai.
    textedit->get_textstream( IMPORTING text = DATA(json_get) ).
    cl_gui_cfw=>flush( ).
    json->* = json_get.
    CASE ucomm.
      WHEN 'SPOS'.
        exit( ).
    ENDCASE.
  ENDMETHOD.

  METHOD run.
    DATA(st_source_code) = ``.
    CALL TRANSFORMATION id SOURCE XML json->* RESULT XML DATA(json_xml) OPTIONS xml_header = 'no'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_ui.
    ENDIF.
    CALL TRANSFORMATION zjsonxtra_json_to_st SOURCE XML json->* RESULT XML st_source_code OPTIONS xml_header = 'no' PARAMETERS indent = 'yes'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_ui.
    ENDIF.
    lcl_r3tr_xtra=>create_update_r3tr_xtra_object(
        i_xsltname    = 'ZJSONXTRA_TEST'
        i_xslt_source = st_source_code ).
    COMMIT WORK AND WAIT.

    DATA(abap_source_code) = ``.
    CALL TRANSFORMATION zjsonxtra_json_to_types SOURCE XML json->* RESULT XML abap_source_code.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_ui.
    ENDIF.
    SPLIT abap_source_code AT |\r\n| INTO TABLE DATA(abap_source_code_table).
    abap_source_code_table = VALUE #(
        LET temp = abap_source_code_table IN
        ( |PROGRAM.| )
        ( |FORM main USING JSON2 TYPE STRING.| )
        ( |BREAK-POINT.| )
        ( LINES OF temp )
        ( |CALL TRANSFORMATION ZJSONXTRA_TEST SOURCE XML JSON2 RESULT ABAPROOT = JSON.| )
        ( |ENDFORM.| ) ).
    GENERATE SUBROUTINE POOL abap_source_code_table NAME DATA(subr_pool_name).
    IF subr_pool_name IS INITIAL.
      RAISE EXCEPTION TYPE lcx_ui.
      " generation/syntax error
      RETURN.
    ENDIF.
    COMMIT WORK AND WAIT.

    PERFORM main IN PROGRAM (subr_pool_name) USING json->*.

  ENDMETHOD.

  METHOD exit.
    IF textedit IS BOUND.
      textedit->free( ).
      FREE textedit.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

PARAMETERS json TYPE string.

DATA ui TYPE REF TO lcl_ui.
TABLES sscrfields.

LOAD-OF-PROGRAM.
  ui = NEW lcl_ui( ).

AT SELECTION-SCREEN OUTPUT.
  TRY.
      ui->pbo( ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

AT SELECTION-SCREEN.
  TRY.
      ui->pai( sscrfields-ucomm ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'E'.
  ENDTRY.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  ui->exit( ).

START-OF-SELECTION.
  TRY.
      ui->run( ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
