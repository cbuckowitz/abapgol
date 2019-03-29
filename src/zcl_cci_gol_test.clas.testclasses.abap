*"* use this source file for your ABAP unit test classes

CLASS tcl_field DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO zcl_cci_gol.

    METHODS test_initial_field_get_raises FOR TESTING.
    METHODS test_initial_field_set_raises FOR TESTING.
    METHODS test_init_field_rows FOR TESTING.

    METHODS test_count_empty_neighborhood FOR TESTING.

    METHODS test_count_neighborhood FOR TESTING.

    METHODS test_future_computation FOR TESTING.

    METHODS test_play_a_round FOR TESTING.

    METHODS setup.

ENDCLASS.


CLASS tcl_field IMPLEMENTATION.


  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.




  METHOD test_initial_field_get_raises.

    TRY.

        mo_cut->get_alive( iv_x = 1 iv_y = 1 ).

        cl_abap_unit_assert=>fail( msg = 'Lesen aus initialer Tabelle darf nicht möglich sein' ).


      CATCH cx_static_check.

    ENDTRY.


  ENDMETHOD.

  METHOD test_initial_field_set_raises.
    TRY.

        mo_cut->set_alive( iv_x = 1 iv_y = 1 iv_alive = abap_true ).

        cl_abap_unit_assert=>fail( msg = 'Schreiben in initiale Tabelle darf nicht möglich sein' ).


      CATCH cx_static_check.

    ENDTRY.

  ENDMETHOD.


  METHOD test_init_field_rows.
    TRY.

        DATA(lv_width) = 10.
        DATA(lv_height) = 10.

        mo_cut->init_field( width = lv_width height = lv_height ).

        cl_abap_unit_assert=>assert_equals(
            act = mo_cut->get_cell_count(  )
            exp = lv_width * lv_height
            msg = 'Schreiben in initiale Tabelle darf nicht möglich sein'
         ).


      CATCH cx_static_check.

    ENDTRY.

  ENDMETHOD.




  METHOD test_count_empty_neighborhood.

    DATA(lv_width) = 10.
    DATA(lv_height) = 10.

    DATA(lv_x) = 0.
    DATA(lv_y) = 0.

    mo_cut->init_field( width = lv_width height = lv_height ).

    DO lv_width TIMES.
      ADD 1 TO lv_x.
      CLEAR lv_y.

      DO lv_height TIMES.
        ADD 1 TO lv_y.

        cl_abap_unit_assert=>assert_equals(
        msg = 'Es dürfen keine Nachbarn in einer leeren Umgebung gezählt werden'
            exp = 0
            act = mo_cut->count_neighbors( iv_x = lv_x iv_y = lv_y ) ).

      ENDDO.
    ENDDO.


    mo_cut->set_alive( iv_x = 3 iv_y = 3 iv_alive = abap_true ).

    cl_abap_unit_assert=>assert_equals(
    msg = 'Es dürfen keine Nachbarn einer Zelle in einer leeren Umgebung gezählt werden'
        exp = 0
        act = mo_cut->count_neighbors( iv_x = 3 iv_y = 3 ) ).


  ENDMETHOD.


  METHOD test_count_neighborhood.

    DATA(lv_width) = 10.
    DATA(lv_height) = 10.

*    x--
*    --x

    mo_cut->init_field( width = lv_width height = lv_height ).
    mo_cut->set_alive( iv_x = 1 iv_y = 1 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 3 iv_y = 2 iv_alive = abap_true ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 32 n(11) = 0'
        exp = 0
        act = mo_cut->count_neighbors( iv_x = 1 iv_y = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 32 n(12) = 1'
        exp = 1
        act = mo_cut->count_neighbors( iv_x = 1 iv_y = 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 32 n(22) = 2'
        exp = 2
        act = mo_cut->count_neighbors( iv_x = 2 iv_y = 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 32 n(31) = 1'
        exp = 1
        act = mo_cut->count_neighbors( iv_x = 3 iv_y = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 32 n(13) = 0'
        exp = 0
        act = mo_cut->count_neighbors( iv_x = 1 iv_y = 3 ) ).

  ENDMETHOD.


  METHOD test_future_computation.

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(0) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 0 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(1) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(2) future true'
        exp = abap_true
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(3) future true'
        exp = abap_true
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 3 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(4) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 4 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(5) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 5 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(6) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 6 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(7) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 7 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'alive n(8) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_true iv_neighbors = 8 ) ).


    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(0) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 0 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(1) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(2) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(3) future true'
        exp = abap_true
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 3 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(4) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 4 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(5) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 5 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(6) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 6 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(7) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 7 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'dead n(8) future false'
        exp = abap_false
        act = mo_cut->compute_future( iv_alive = abap_false iv_neighbors = 8 ) ).


  ENDMETHOD.


  METHOD test_play_a_round.

    DATA(lv_width) = 10.
    DATA(lv_height) = 10.

*    x--
*    --x

    mo_cut->init_field( width = lv_width height = lv_height ).
    mo_cut->set_alive( iv_x = 2 iv_y = 1 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 4 iv_y = 1 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 5 iv_y = 1 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 1 iv_y = 2 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 3 iv_y = 2 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 4 iv_y = 2 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 5 iv_y = 2 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 2 iv_y = 3 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 3 iv_y = 4 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 4 iv_y = 4 iv_alive = abap_true ).
    mo_cut->set_alive( iv_x = 4 iv_y = 5 iv_alive = abap_true ).

    mo_cut->play_a_round( ).

    cl_abap_unit_assert=>assert_equals(
        msg = '11 dead'
        exp = abap_false
        act = mo_cut->get_alive( iv_x = 1 iv_y = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '21 alive'
        exp = abap_true
        act = mo_cut->get_alive( iv_x = 2 iv_y = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = '31 alive'
        exp = abap_true
        act = mo_cut->get_alive( iv_x = 3 iv_y = 1 ) ).


  ENDMETHOD.

ENDCLASS.
