PROC GetPaletsToMachine(byte _Mosaic, byte _Cinta)!num tipus,robtarget pallet

    BackToZero(1);
    BackToZero(2);
    !Reset if needed

    TEST _Mosaic
    CASE 1:
        IF LastPaletPos{_Cinta,3}>=0 THEN !si la ultima pos de la cinta en eix z es o




        IF LastPaletPos{_Cinta,3}>=0 THEN
            IF LastPaletPos{_Cinta,2}<=1 THEN
                IF _Cinta=1 AND CountMateriaP{1}+1<=SizeMosaic{1} THEN
                    TPWrite "Coge pieza "+Mosaic1{CountMateriaP{1}+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND CountMateriaP{2}+1<=Dim(Materia1,1) THEN
                    TPWrite "Coge pieza "+Mosaic1{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move Mc,pallet,materiaC{1}/2,materiaC{2}/2+materiaC{2}*LastPaletPos{_Cinta,2},materiaC{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasC<wC THEN
                    piezasC:=piezasC+1;
                ENDIF
                contarNumPiezas pallet;
                showNumPallet;
                LastPaletPos{_Cinta,2}:=LastPaletPos{_Cinta,2}+1;

                RETURN ;
            ENDIF

            IF LastPaletPos{_Cinta,1}<=1 THEN
                IF _Cinta=1 AND CountMateriaP{1}+1<=Dim() THEN
                    TPWrite "Coge pieza "+patronMosaic1{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic1{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move Ma,pallet,materiaC{1}+materiaA{1}/2+materiaA{1}*LastPaletPos{_Cinta,1},materiaA{2}/2,materiaA{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasA<wA THEN
                    piezasA:=piezasA+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;

                IF LastPaletPos{_Cinta,1}>=2 THEN
                    LastPaletPos{_Cinta,3}:=LastPaletPos{_Cinta,3}-1;
                    LastPaletPos{_Cinta,1}:=0;
                    LastPaletPos{_Cinta,2}:=0;
                ENDIF

                RETURN ;
            ENDIF
            RETURN ;
        ENDIF

    CASE 2:
        IF LastPaletPos{_Cinta,3}>=0 THEN
            IF LastPaletPos{_Cinta,1}<=1 THEN
                IF LastPaletPos{_Cinta,2}<=1 THEN
                    IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                        TPWrite "Coge pieza "+patronMosaic2{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                    ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                        TPWrite "Coge pieza "+patronMosaic2{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                    ENDIF
                    move Mc,pallet,materiaC{1}/2+materiaC{1}*LastPaletPos{_Cinta,1},materiaC{2}/2+materiaC{2}*LastPaletPos{_Cinta,2},materiaC{3}*LastPaletPos{_Cinta,3};
                    WaitTime 0.5;
                    IF piezasC<wC THEN
                        piezasC:=piezasC+1;
                    ENDIF

                    contarNumPiezas pallet;
                    showNumPallet;

                    LastPaletPos{_Cinta,2}:=LastPaletPos{_Cinta,2}+1;
                    IF LastPaletPos{_Cinta,2}>=2 THEN
                        LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;
                        LastPaletPos{_Cinta,2}:=0;
                    ENDIF
                    IF LastPaletPos{_Cinta,1}>=2 THEN
                        LastPaletPos{_Cinta,3}:=LastPaletPos{_Cinta,3}-1;
                        LastPaletPos{_Cinta,1}:=0;
                    ENDIF

                    RETURN ;
                ENDIF
            ENDIF
        ENDIF

    CASE 3:
        IF LastPaletPos{_Cinta,3}>=0 THEN
            IF LastPaletPos{_Cinta,2}<=3 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=15 THEN
                    TPWrite "Coge pieza "+patronMosaic3{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=15 THEN
                    TPWrite "Coge pieza "+patronMosaic3{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move Mb,pallet,materiaB{2}/2,materiaB{1}/2+materiaB{1}*LastPaletPos{_Cinta,2},materiaB{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasB<wB THEN
                    piezasB:=piezasB+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,2}:=LastPaletPos{_Cinta,2}+1;
                RETURN ;
            ENDIF

            IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                TPWrite "Coge pieza "+patronMosaic3{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
            ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                TPWrite "Coge pieza "+patronMosaic3{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
            ENDIF
            move Ma,pallet,materiaB{2}+materiaA{1},materiaA{2}/2,materiaA{3}*LastPaletPos{_Cinta,3};
            WaitTime 0.5;
            IF piezasA<wA THEN
                piezasA:=piezasA+1;
            ENDIF

            contarNumPiezas pallet;
            showNumPallet;

            IF LastPaletPos{_Cinta,2}>=2 THEN
                LastPaletPos{_Cinta,3}:=LastPaletPos{_Cinta,3}-1;
                LastPaletPos{_Cinta,2}:=0;
            ENDIF

            RETURN ;
        ENDIF

    CASE 4:
        !Todos estos IF que hay a continuaci�n son necesatios para que retome el movimiento de este mosaico donde lo dej�. El  LastPaletPos{_Cinta,1}
        !es como una eLastPaletPosecie de contador, y se usa esto y no una variable _Cintailiar nueva porque es lo que se resetea en la funcion de escojer mosaic

        !Pieza B derecha arriba
        IF LastPaletPos{_Cinta,3}>=0 THEN
            IF LastPaletPos{_Cinta,1}=0 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move Mb,pallet,materiaB{2}/2,materiaB{2}+materiaB{1}/2,materiaB{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasB<wB THEN
                    piezasB:=piezasB+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;
                RETURN ;
            ENDIF

            !Pieza B izquierda arriba
            IF LastPaletPos{_Cinta,1}=1 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move Mb,pallet,materiaB{1}/2,materiaB{2}/2,materiaB{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasB<wB THEN
                    piezasB:=piezasB+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;
                p0.rot:=rotOriginal;
                RETURN ;
            ENDIF

            !Pieza C
            IF LastPaletPos{_Cinta,1}=2 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                move MC,pallet,materiaB{1}+materiaC{1}/2,materiaB{1}+materiaC{1}/2,materiaC{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasC<wC THEN
                    piezasC:=piezasC+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;
                RETURN ;
            ENDIF

            !Pieza B derecha abajo
            IF LastPaletPos{_Cinta,1}=3 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                p0.rot:=rot90;
                move Mb,pallet,materiaB{1}+materiaC{1}+materiaB{1}/2,materiaB{1}+materiaB{2}/2,materiaB{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasB<wB THEN
                    piezasB:=piezasB+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;
                p0.rot:=rotOriginal;
                RETURN ;
            ENDIF

            !Pieza B izquierda abajo
            IF LastPaletPos{_Cinta,1}=4 THEN
                IF _Cinta=1 AND numPiezasCojidaLastPaletPos0+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos0+1}+" del pallet 0";
                ELSEIF _Cinta=2 AND numPiezasCojidaLastPaletPos1+1<=12 THEN
                    TPWrite "Coge pieza "+patronMosaic4{numPiezasCojidaLastPaletPos1+1}+" del pallet 1";
                ENDIF
                p0.rot:=rot90;
                move Mb,pallet,materiaB{1}+materiaB{2}/2,materiaB{1}/2,materiaB{3}*LastPaletPos{_Cinta,3};
                WaitTime 0.5;
                IF piezasB<wB THEN
                    piezasB:=piezasB+1;
                ENDIF

                contarNumPiezas pallet;
                showNumPallet;

                LastPaletPos{_Cinta,1}:=LastPaletPos{_Cinta,1}+1;

                IF LastPaletPos{_Cinta,1}>=5 THEN
                    LastPaletPos{_Cinta,3}:=LastPaletPos{_Cinta,3}+1;
                    LastPaletPos{_Cinta,1}:=0;
                ENDIF
                RETURN ;
            ENDIF
        ENDIF
    ENDTEST
    ENDPROC