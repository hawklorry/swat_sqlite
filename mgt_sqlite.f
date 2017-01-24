      subroutine mgt_sqlite(id_op,id_hru)

        use parm

        integer,intent(in) :: id_op
        integer,intent(in) :: id_hru
        j = id_hru

        if(imgt == 0) return;

        if(id_op == 10) return;
        if(id_op == 11) return;
        if(id_op == 14) return;
        if(id_op == 15) return;

        call sqlite3_set_column( colmgt(1), j )
        call sqlite3_set_column( colmgt(2), iyr )
        call sqlite3_set_column( colmgt(3), i_mo )
        call sqlite3_set_column( colmgt(4), icl(iida))
        !!for end-of-year processes,see simulate.f
        !!when operatn is executed, the iida is already updated, but year is not updated.
        !!in this case, use the last day instead
        if(i_mo == 1 .AND. iida > 365) then
            call sqlite3_set_column( colmgt(3), 12)
            call sqlite3_set_column( colmgt(4), iida-1-ndays(12))
        end if
        call sqlite3_set_column( colmgt(5), "" )
        !call sqlite3_set_column( colmgt(6), soperation )

        call sqlite3_set_column( colmgt(7), phubase(j) )
        call sqlite3_set_column( colmgt(8), phuacc(j) )
        call sqlite3_set_column( colmgt(9), sol_sw(j) )
        call sqlite3_set_column( colmgt(10), bio_ms(j) )
        call sqlite3_set_column( colmgt(11), sol_rsd(1,j) )
        call sqlite3_set_column( colmgt(12), sol_sumno3(j) )
        call sqlite3_set_column( colmgt(13), sol_sumsolp(j) )
        call sqlite3_set_column( colmgt(14), 0.0 ) !!yield for harvest/kill
        call sqlite3_set_column( colmgt(15), 0.0 ) !!mix efficiency

        call sqlite3_set_column( colmgt(16), 0.0 ) !!6 fertilizer
        call sqlite3_set_column( colmgt(17), 0.0 )
        call sqlite3_set_column( colmgt(18), 0.0 )
        call sqlite3_set_column( colmgt(19), 0.0 )
        call sqlite3_set_column( colmgt(20), 0.0 )
        call sqlite3_set_column( colmgt(21), 0.0 )

        call sqlite3_set_column( colmgt(22), 0.0 ) !!pesticide

        call sqlite3_set_column( colmgt(23), 0.0 ) !!5 stress for harvest/kill
        call sqlite3_set_column( colmgt(24), 0.0 )
        call sqlite3_set_column( colmgt(25), 0.0 )
        call sqlite3_set_column( colmgt(26), 0.0 )
        call sqlite3_set_column( colmgt(27), 0.0 )

        call sqlite3_set_column( colmgt(28), 0.0 ) !!Harvest yield
        call sqlite3_set_column( colmgt(29), 0.0 )
        call sqlite3_set_column( colmgt(30), 0.0 )
        call sqlite3_set_column( colmgt(31), 0.0 )
        call sqlite3_set_column( colmgt(32), 0.0 )
        call sqlite3_set_column( colmgt(33), 0.0 )

        call sqlite3_set_column( colmgt(34), 0.0 ) !!manure

        call sqlite3_set_column( colmgt(35), 0.0 ) !!irrigation
        call sqlite3_set_column( colmgt(36), 0 )
        call sqlite3_set_column( colmgt(37), 0 )
        select case (id_op)
            case (1)
                call sqlite3_set_column( colmgt(5), cpnm(idplt(j)) )
                call sqlite3_set_column( colmgt(6), "PLANT" )
            case (2) !!irrigation
                call sqlite3_set_column( colmgt(6), "IRRIGATE" )
                call sqlite3_set_column( colmgt(36), irr_sc(j) )
                call sqlite3_set_column( colmgt(37), irr_no(j) )
            case (3) !!fertilizer
                call sqlite3_set_column( colmgt(5), fertnm(ifrttyp) )
                call sqlite3_set_column( colmgt(6), "FERT" )
                call sqlite3_set_column( colmgt(16), frt_kg )
                call sqlite3_set_column( colmgt(17), fertno3 )
                call sqlite3_set_column( colmgt(18), fertnh3 )
                call sqlite3_set_column( colmgt(29), fertorgn )
                call sqlite3_set_column( colmgt(20), fertsolp )
                call sqlite3_set_column( colmgt(21), fertorgp )
            case (4) !!pesticide
                call sqlite3_set_column( colmgt(5), pname(ipest) )
                call sqlite3_set_column( colmgt(6), "PEST" )
                call sqlite3_set_column( colmgt(22), pst_kg )
            case (5) !! harvest and kill operation
                call sqlite3_set_column( colmgt(5), cpnm(idplt(j)) )
                call sqlite3_set_column( colmgt(6), "HARV/KILL" )
                call sqlite3_set_column( colmgt(14), yield )
                call sqlite3_set_column( colmgt(23), strsn_sum(j) ) !!5 stress for harvest/kill
                call sqlite3_set_column( colmgt(24), strsp_sum(j) )
                call sqlite3_set_column( colmgt(25), strstmp_sum(j) )
                call sqlite3_set_column( colmgt(26), strsw_sum(j) )
                call sqlite3_set_column( colmgt(27), strsa_sum(j) )
            case (6) !!tillage
                call sqlite3_set_column( colmgt(5), tillnm(idtill) )
                call sqlite3_set_column( colmgt(6), "TILLAGE" )
                call sqlite3_set_column( colmgt(15), effmix(idtill) )
            case (7) !!Harvest only
                call sqlite3_set_column( colmgt(5), cpnm(idplt(j)) )
                call sqlite3_set_column( colmgt(6), "HARVEST ONLY" )
                call sqlite3_set_column( colmgt(14), yield )

                call sqlite3_set_column( colmgt(23), strsn_sum(j) ) !!5 stress for harvest/kill
                call sqlite3_set_column( colmgt(24), strsp_sum(j) )
                call sqlite3_set_column( colmgt(25), strstmp_sum(j) )
                call sqlite3_set_column( colmgt(26), strsw_sum(j) )
                call sqlite3_set_column( colmgt(27), strsa_sum(j) )

                call sqlite3_set_column( colmgt(28), yieldgrn )
                call sqlite3_set_column( colmgt(29), yieldbms )
                call sqlite3_set_column( colmgt(30), yieldtbr )
                call sqlite3_set_column( colmgt(31), yieldrsd )
                call sqlite3_set_column( colmgt(32), yieldn )
                call sqlite3_set_column( colmgt(33), yieldp )
            case (8) !!kill
                call sqlite3_set_column( colmgt(6), "KILL" )
            case (9) !!grazing
                call sqlite3_set_column( colmgt(6), "GRAZE" )
                call sqlite3_set_column( colmgt(34), manure_kg(j))
            case (10) !!auto irrigation
            case (11) !!auto fertilizer
            case (12) !!street sweeping (only if iurban=2)
                call sqlite3_set_column( colmgt(6), "STREET SWEEP" )
            case (13) !!release/impound water in rice fields
                call sqlite3_set_column( colmgt(6), "RELEASE/IMPOUND" )
            case (14) !!continuous fertilization
            case (15) !!continuous pesticide
            case (16) !!burning
                call sqlite3_set_column( colmgt(6), "BURN" )
            !!following codes are not real code
            case (17) !!start-dorm
                call sqlite3_set_column( colmgt(5), cpnm(idplt(j)) )
                call sqlite3_set_column( colmgt(6), "START-DORM" )
            case (18) !!end-dorm
                call sqlite3_set_column( colmgt(5), cpnm(idplt(j)) )
                call sqlite3_set_column( colmgt(6), "END-DORM" )
            case (19) !!continuous fertilization
                call sqlite3_set_column( colmgt(16), cfrt_kg(j) )
                call sqlite3_set_column( colmgt(6), "CONT FERT" )
            case (20) !!continuous pesticide
                call sqlite3_set_column( colmgt(22), cpst_kg(j) )
                call sqlite3_set_column( colmgt(6), "CONT PEST" )
            case (21) !!auto irrigation
                call sqlite3_set_column( colmgt(6), "AUTOIRR" )
                call sqlite3_set_column( colmgt(35), aird(j) )
                call sqlite3_set_column( colmgt(36), irrsc(j) )
                call sqlite3_set_column( colmgt(37), irrno(j) )
            case (22) !!auto fertilizer
                call sqlite3_set_column( colmgt(6), "AUTOFERT" )
                call sqlite3_set_column( colmgt(16), frt_kg )
                call sqlite3_set_column( colmgt(17), fertno3 )
                call sqlite3_set_column( colmgt(18), fertnh3 )
                call sqlite3_set_column( colmgt(19), fertorgn )
                call sqlite3_set_column( colmgt(20), fertsolp )
                call sqlite3_set_column( colmgt(21), fertorgp )
        end select

        call sqlite3_insert_stmt( db, stmtmgt, colmgt )

      end
