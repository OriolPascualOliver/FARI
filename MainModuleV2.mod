MODULE MainModule
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   CODI FASE 3 jdrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
    !
    !
    ! date
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !0) definicio i ini Vars
    !Ho sento Oriol del futur :(

    !--------------------   CINEMATIC DEFINITIONS --------------------
    CONST robtarget HOME:=[[500,-10,700],[0.714516,0.0199544,0.699323,0.00399707],[-1,0,0,1],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget Ppalet1:=[[650,175,160],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget Ppalet2:=[[450,175,160],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO1:=[[-10,-500,300],[0.0487016,-0.732289,-0.678123,0.0391261],[-2,-1,0,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO2:=[[-50,500,60],[0.0545846,0.782406,-0.617839,0.0560026],[1,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO3:=[[-500,100,125],[0.00252886,0.999052,-0.0406592,0.015331],[1,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PDiscard:=[[400,500,100],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    !Vars de posicio'
    CONST num offset_Ppalet{3} := [0,-10,-10];
    CONST num offset_PUNTO{3} := [-10, -10, -10];
 
    CONST num Zoffset := 100; !PUNT SEGURETAT VERTICAL
    !Vars per contar els offsets i les distancies
    VAR num LastPaletPos{2,3}:=[[0,0,2], [0,0,2]]; !NOTE: z=2 ja que hi han 2 nivells per treure i comenca des de dalt, quan z=0, ja s'han tret els 2 pisos de coses
    !array per guardar on es queda a despaletitzar {Last position as NPalet, XYZ}
    VAR bool RstLastPaletPos{2}:=[FALSE, FALSE];
    !array boleana per guardar l'estat del reset de la memoria de despaletitzar
    !--------------------  DATA  --------------------
    CONST byte SizeItem{3,3}:=[[10,40,20], [10,30,20], [20,20,20]];
    !Array dimensions materies, index 1->A, 3->C
    VAR byte Mosaic1{12}:=[3, 3, 1, 1, 3, 3, 1, 1, 3, 3, 1, 1]; !C, C, A, A, C, C, A, A, C, C, A, A
    VAR byte Mosaic2{12}:=[3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]; !C, C, C, C, C, C, C, C, C, C, C, C
    VAR byte Mosaic3{15}:=[2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1]; !B, B, B, B, A, B, B, B, B, A, B, B, B, B, A
    VAR byte Mosaic4{15}:=[2, 2, 2, 2, 3, 2, 2, 2, 2, 3, 2, 2, 2, 2, 3]; !B, B, B, B, C, B, B, B, B, C, B, B, B, B, C
    !VAR byte SizeMosaic{4}:=[Dim(Mosaic1, 1), Dim(Mosaic2, 1), Dim(Mosaic3, 1), Dim(Mosaic4, 1)];
    VAR byte SizeMosaic{4}:=[12, 12, 15, 15];
    !array per guardar patrons de peces a mosaic CW+C, + numero items (s'hagues pogut posar en una super matriu al estilo matrix btw)3

    VAR byte PosMosaic{4,15,3}; ![x,y,z], [pos], [mosaic] TODO: revisar index posmosaic!
    ! a mega array for each position fuck yeah imagine that array (la controladora te 2gb de memoria i 1 de DRAM aixi que pot guardar 180 bytes)

    !--------------------  VARS  --------------------
    VAR bool Run :=TRUE;
    !bool per iniciar i parar el proces {Guarda l estat del palet}
    VAR num materia{3};
    !array per guardar qty de materia per caaçda matina

    VAR num CountMateriaM{3}:=[0,0,0];
    VAR num CountMateriaP{2}:=[0,0];
    !array contar materies entrades a maquina i palet 

    VAR num auxiliar{3}:=[0,0,0];
    !array aux

    VAR bool FlagNoStock:=False;
    !Bool per buscar un altre palet per agafar peces

    VAR bool FlagPaletWithStock{2}:=[FALSE, FALSE];
    !array bool per guardar si hi han peces al palet

    VAR intnum despaletitzar; 
    !var per guardar la info de despaletitzar

    VAR byte Mosaic{2}:=[0,0];
    !array per guardar el tipus de mosaic [Cinta1, Cinta2]

    VAR bool DetectCinta1:=FALSE;
    VAR bool DetectCinta2:=FALSE;
    !bools on guardar l estat dels sensors de presencia dels palets

    VAR intnum PwrCinta1;
    VAR intnum PwrCinta2;
    !vars per iniciar cintes i fer entrar palets -INTERRUPTS

    VAR bool RequestPalet{2}:=[FALSE, FALSE];
    !Vars per guardar de forma rapida si s ha demanat un palet

    VAR num Priority{3}:=[0,0,0];
    !array per guardar prioritats maquina 1-3

    VAR byte ComputePriority{3}:=[0,0,0];
    !array per guardar l ordre d agafar peces
    
    VAR byte CmpMachineOut;
    !Var sortida de compute machine (no tira el return)
    
    VAR bool OutputLogs:=FALSE;
    !VAR to enable output logs
    
    VAR byte State{3}:=[1,2,3];
    !var per guardar quina cinta esta activa

    VAR byte CountPaletCmplt:=0;
    !var per guardar quina iteracio de item a maquina es (valor ini=0 per determinar que encara no ha comencat)
    
    !------------ VARS "Privades" ------------
    VAR byte PvtAux1:=0; !func 1
    VAR byte PvtAux2:=0;
    VAR byte PvtAux3:=0; !byte detecio cinta


!1) Main func
PROC Main()
  TPErase;  !borrar pantalla
  GoToHome; !anar a Home pos

  ! Gestiona l entrada de dades de l usuari
  EnterNElements;
  CheckNElements;
  CmpPty;
  FillPosMosaic("CoolWay");

  CONNECT PwrCinta1 WITH Trp_Cinta1; !TODO: solve this
  ISignalDI Palet1,1,PwrCinta1;

  CONNECT PwrCinta2 WITH Trp_Cinta2;
  ISignalDI Palet2,1,PwrCinta2;
  !activa 2 cintes
  SetDO EnCinta1, 1;
  SetDO EnCinta2, 1;
  !es desactiven amb la interrupció de la deteció de la cinta
  
  
  WHILE Run DO
    WHILE (DetectCinta1 OR DetectCinta2) DO !mentres hi hagi algun palet
        GetMateriaMk1; !agafa la materia !TODO: mirar nom func
        TPUI;
    ENDWHILE
    !mirar quin falta i demanar palet
    
  ENDWHILE
  TPErase;


ENDPROC


!2) Funcs aux

PROC EnterNElements()
!Func per entrer els elements per materia
  VAR num resposta;
  VAR bool TrucoEN:=FALSE;
  VAR num i:=1;
  !FOR i FROM 1 TO 3 DO
  WHILE i < 4 DO
      TPErase;
      TPWrite "MAQUINA "\Num:=i;
      TPReadNum resposta,"Enter element count: ";
      IF resposta = 69420 THEN
        !Srooy in advanced, function para probar como si fuera GTA
        materia:=[1,2,3];
        priority:=[1,2,3];
        TPWrite "-------Truco UNLOCKED-------";
        GOTO TrucoGTA;
      ELSEIF resposta > 0 THEN
        materia{i}:=resposta;
      ELSE
        TPWrite "ENTER A VALID INPUT STUPID :( ";
        i:=i-1;
      ENDIF
      i:=i+1;
  !ENDFOR
  ENDWHILE


  !entrar prioritats
  !FOR i FROM 1 TO 3 DO
  WHILE i < 7 DO
      TPErase;
      TPWrite "MAQUINA "\Num:=i-3;
      TPReadNum resposta,"Enter priority level (form 1 to 3 being 1 the highest) pls: ";
      IF resposta > 0 AND resposta < 4 THEN
        priority{i-3}:=resposta;
      ELSE
        TPWrite "ENTER A VALID INPUT STUPID :( ";
        WaitTime 1.5;
        i:=i-1;
      ENDIF
    i:=i+1;
  ENDWHILE
  !ENDFOR
  TrucoGTA:
ENDPROC

PROC CheckNElements()
!Func per validar les dades entrades
ShowMe "<CheckNElements>";
  auxiliar{1}:=(Priority{1} * priority{2} * priority{3}); !hem fet el producte per veure si tenen nums de prty diferents i els adequats, i m a genius tnku
  WHILE( auxiliar{1} <> 6) DO 
    TPWrite "ENTER A VALID PRIORITY INPUT YOU BASTARD :(";
    WaitTime 1.5;
    EnterNElements;
    auxiliar{1}:=(Priority{1} * priority{2} * priority{3});
  ENDWHILE
  ShowMe "</CheckNElements>";
    
ENDPROC

PROC CmpPty()
! Func que calcula la prioritat d una manera super pro
  ! aux 1->3
  ShowMe "<CmpPty>";
  auxiliar{2}:=100*priority{1}+10*priority{2}+priority{3};
  TEST auxiliar{2}
    CASE 123:
      ComputePriority := [1,2,3];
    CASE 132:
      ComputePriority := [1,2,3];
    CASE 213:
      ComputePriority := [2,1,3];
    CASE 231:
      ComputePriority := [3,1,2];
    CASE 312:
      ComputePriority := [2,3,1];
    CASE 321:
      ComputePriority := [3,2,1];
    DEFAULT:
      err;
  ENDTEST
  ShowMe "</CmpPty>";
ENDPROC



PROC GetMateriaMk1() !NEW VERSION OF GETMATERIA
    !Here is where the magic happens btch winkwink 
  VAR byte Index:=1;
  VAR byte HighestPriority;
  !CountPaletCmplt guarda quina maquina ha omplert
  ShowMe "<GetMateriaMk1>";
  IF CountPaletCmplt = 0 OR CountPaletCmplt > 3 THEN
    CountPaletCmplt:=1;
    !First Time
  ENDIF
  HighestPriority:= ComputePriority{CountPaletCmplt};

  ShowMe "GetMateria>HigestPriority=";
  ShowMe ByteToStr(HighestPriority);
  WaitTime 0.5;

  !This is to find the cinta w more intresting parts (CASE BOTH 0 HANDELED BY THE FACT THAT THEY ARE EQUAL TO ZEROW madafaka)
  FOR j FROM 1 TO 2 DO !AQUI TENIM BYTE (NO POT SER BYTE; CAL ITERAR)
    TEST Mosaic{j}
    CASE 0:
      !Cinta Not avilable
    CASE 1:
      FOR k FROM 1 TO SizeMosaic{Mosaic{j}} DO
        IF HighestPriority = Mosaic1{k} THEN
          Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    CASE 2:
      FOR k FROM 1 TO SizeMosaic{Mosaic{j}} DO
        IF HighestPriority = Mosaic2{k} THEN
            Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    CASE 3:
      FOR k FROM 1 TO SizeMosaic{Mosaic{j}} DO
        IF HighestPriority = Mosaic3{k} THEN
            Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    CASE 4:
     FOR k FROM 1 TO SizeMosaic{Mosaic{j}} DO
        IF HighestPriority = Mosaic4{k} THEN
            Incr auxiliar{j+1};
        ENDIF
     ENDFOR
    DEFAULT:
    err;
    ENDTEST
    !/CalcAlgorism-> se ha guardado en auxiliar{2i3} la suma de los items mas prioritarios para decidir que cinta pillar


  IF auxiliar{2} > auxiliar{3} AND Mosaic{1} <> 0 THEN
    GetPaletsToMachine 1, 1; !cinta1
    WaitReq;
  ELSEIF auxiliar{2} < auxiliar{3} AND Mosaic{2} <> 0 THEN
    GetPaletsToMachine 2, 1; !cinta 2
    WaitReq; !TODO: finish this (func que espera mentres no hi han palets)
  ELSE
    !Les cintes tenen 0 elements guais
    !demanar mes palets. TODO: demanar palets
  ENDIF
  auxiliar{2}:=0;
  auxiliar{3}:=0;
 ENDFOR
  Incr CountPaletCmplt;
  !Maquina omplerta
  ShowMe "</GetMateria>";
ENDPROC

PROC CmpMachine(byte PvtCinta, num PvtIndex1)

!func que retorna el tipus de materia per trobar la estaico, chorras, FALTA ACABAr
  
  TEST Mosaic{PvtCinta}
  CASE 1:
    PvtAux1:=Mosaic1{PvtIndex1};
  CASE 2:
    PvtAux1:=Mosaic2{PvtIndex1};
  CASE 3:
    PvtAux1:=Mosaic3{PvtIndex1};
  CASE 4:
    PvtAux1:=Mosaic4{PvtIndex1};
  ENDTEST
  CmpMachineOut:= PvtAux1;
ENDPROC







PROC EnterTypeOfMosaic(byte PvtAux3)
!Func per entrer el tipus de mosaic
  VAR num resposta;
  TPErase;
  TPWrite "Palet detectat a Cinta "\Num:=PvtAux3;
  TPReadNum resposta,"Enter mosaic type (from 1 to 4): ";
  IF resposta > 0 AND resposta < 5 THEN
    Mosaic{PvtAux3}:=resposta;
  ELSE
    TPWrite "ENTER A VALID INPUT STUPID :( ";
    WaitTime 1.5;
    EnterTypeOfMosaic PvtAux3;
  ENDIF
  WaitTime 1.5;
 
ENDPROC



PROC GetPaletsToMachine(byte PvtCinta3, byte PvtIndex)
  !Func que donada el num de cinta i el numero de item a treure, el treu i es crida a ella mateixa al acabar fins que el palet esta buit
  !Flipa amb la recursivitat chavaless
    ShowMe "<GetPaletsToMachine>";
    
    
    
    BackToZero(1);
    BackToZero(2);
    !Reset if needed
    !TODO: revisar
    TEST PvtCinta3 
    CASE 1:
        !PosMosaic{(ABCD),(Iteracio),(XYZ)}
        GoToPoint Ppalet1, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 1}, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 2}, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 3};
    CASE 2: 
        GoToPoint Ppalet2, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 1}, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 2}, PosMosaic{Mosaic{PvtCinta3}, PvtIndex, 3};
    ENDTEST
    !va a la cinta que toqui a la pos que toqui

    CmpMachine PvtCinta3, PvtIndex;
    PvtAux2:= CmpMachineOut;
    !Mira quin item esperar

    IF CountMateriaM{PvtAux2} >= materia{PvtAux2} THEN  
        PvtAux2:=4; !Discard
    ENDIF
    
    TEST PvtAux2
    CASE 1:
        GoToP1;
        Incr CountMateriaM{1};
    CASE 2:
        GoToP2;
        Incr CountMateriaM{2};
    CASE 3:
        GoToP3;
        Incr CountMateriaM{3};
    DEFAULT:!discard if already full
        GoToPoint PDiscard, 0, 0, 0;
    ENDTEST

    TPUI;


    !mira si cal recursivitat o el palet esta buit
    IF PvtIndex < SizeMosaic{Mosaic{PvtCinta3}} THEN
        Incr PvtIndex;
        GetPaletsToMachine PvtCinta3, PvtIndex;
    ELSEIF PvtCinta3 = 1 THEN
        !demanar palet 1
        SetDO EnCinta1, 1;
        
        RequestPalet{1}:=TRUE;
    ELSE
        !demanar palet 2
        SetDO EnCinta2, 1;
        RequestPalet{2}:=TRUE;
    ENDIF
ENDPROC

PROC BackToZero(byte PaletNum) 
! proc per resetejar les ultimes posicions amb el flag RST...
  IF RstLastPaletPos{PaletNum}=TRUE THEN
    LastPaletPos{PaletNum,1}:=0;
    LastPaletPos{PaletNum,2}:=0;
    LastPaletPos{PaletNum,3}:=2; !2 or zerow
    RstLastPaletPos{PaletNum}:=FALSE;
  ENDIF
ENDPROC

PROC WaitReq() !TODO: revisar aixo
!Func que va a home si s ha d esperar a que vinguin palets
  IF DetectCinta1=FALSE AND DetectCinta2=FALSE  AND RequestPalet{1} AND RequestPalet{2} THEN
    GoToHome;
    WHILE DetectCinta1 = FALSE AND DetectCinta2 = FALSE DO
      WaitTime 0.5;
    ENDWHILE
  ENDIF
  RequestPalet{1}:= DetectCinta1;
  RequestPalet{2}:= DetectCinta2;

ENDPROC







PROC FillPosMosaic(string s) !TODO: fer macu si tinc temps, Rta: no.
!Func que omple la matriu de posicions
    PosMosaic:=[[[10,10,2],[30,10,2],[20,25,2],[20,35,2],           [10,10,1],[30,10,1],[20,25,1],[20,35,1],          [10,10,0],[30,10,0],[20,25,0],[20,35,0],  [0,0,0],[0,0,0],[0,0,0]],
                [[10,10,2],[30,10,2],[10,30,2],[30,30,2],           [10,10,1],[30,10,1],[10,30,1],[30,30,1],          [10,10,0],[30,10,0],[10,30,0],[30,30,0],  [0,0,0],[0,0,0],[0,0,0]],
                [[5,15,2],[15,15,2],[25,15,2],[35,15,2],[20,35,2],  [5,15,1],[15,15,1],[25,15,1],[35,15,1],[20,35,1], [5,15,0],[15,15,0],[25,15,0],[35,15,0],[20,35,0]],
                [[5,15,2],[35,15,2],[25,35,2],[5,25,2],[20,20,2],   [5,15,1],[35,15,1],[25,35,1],[5,25,1],[20,20,1],  [5,15,0],[35,15,0],[25,35,0],[5,25,0],[20,20,0]]];
    

    
ENDPROC

PROC TPUI()
!TPUI teach pendant User Interface PAT PENDING (R) TM

  TPErase;
  TPWrite " --------------------------------------";
  TPWrite " Suma peces total estacio 1: "\Num:=materia{1};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{1};
  TPWrite " Falten per entrar: "\Num:=materia{1}-CountMateriaM{1};
  TPWrite " --------------------------------------";
  TPWrite " Suma peces total estacio 2: "\Num:=materia{2};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{2};
  TPWrite " Falten per entrar: "\Num:=materia{2}-CountMateriaM{2};
  TPWrite " --------------------------------------";
  TPWrite " Suma peces total estacio 3: "\Num:=materia{3};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{3};
  TPWrite " Falten per entrar: "\Num:=materia{3}-CountMateriaM{3};
ENDPROC

PROC ShowMe(string msg)
!FUNC per debugging
  IF OutputLogs THEN
  TPWrite msg;
  WaitTime 0.5;
  ENDIF
ENDPROC

PROC err()
!something went really wrong bro
  TPWrite "-- ERROR 418 I'm a teapot. Can t'make cofee :/ --";                  
  Stop;
ENDPROC

!Traps !TODO:SOLVE ARRAY
  TRAP Trp_Cinta1 !neets update
    DetectCinta1:=TRUE;
    FlagPaletWithStock{1}:=TRUE;
    SetDO EnCinta1, 0;
    RequestPalet{1}:=FALSE;
    EnterTypeOfMosaic 1;
  ENDTRAP

  TRAP Trp_Cinta2
    DetectCinta2:=TRUE;
    FlagPaletWithStock{2}:=TRUE;
    SetDO EnCinta2, 0;
    RequestPalet{2}:=FALSE;
    EnterTypeOfMosaic 2;
  ENDTRAP !end missing

!Funcs aborrides de moviments a maquines i punts i tal
!{GoToPoint(punt, offset), GoToHome, GoToP1, GoToP2, GoToP3}
  PROC GoToPoint(robtarget PvtPTG, byte PvtOFST1, byte PvtOFST2, byte PvtOFST3) !found num, expected dim aixi que poso vars individuals i ja
    !INPUT: PointToGo
      !func que va qualsevol pos del palet de manera guai
      MoveJ Offs(PvtPTG, 10*offset_Ppalet{1}+PvtOFST1, 10*offset_Ppalet{2}+PvtOFST2, 10*offset_Ppalet{3}+Zoffset+PvtOFST3), v500, fine, tool0;
      MoveL Offs(PvtPTG, 10*offset_Ppalet{1}+PvtOFST1, 10*offset_Ppalet{2}+PvtOFST2, 10*offset_Ppalet{3}+PvtOFST3), v100, fine, tool0;
      MoveL Offs(PvtPTG, 10*offset_Ppalet{1}+PvtOFST1, 10*offset_Ppalet{2}+PvtOFST2, 10*offset_Ppalet{3}+Zoffset+PvtOFST3), v100, fine, tool0;
  ENDPROC
  
  PROC GoToHome()
      MoveJ home,v500,fine,tool0;
  ENDPROC
  PROC GoToP1()
      !func que va a maquina 1
      MoveJ Offs(PUNTO1, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v500, fine, tool0;
      MoveL Offs(PUNTO1, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}), v100, fine, tool0;
      MoveL Offs(PUNTO1, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v100, fine, tool0;
  ENDPROC
  PROC GoToP2()
      !func que va a maquina 2
      MoveJ Offs(PUNTO2, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v500, fine, tool0;
      MoveL Offs(PUNTO2, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}), v100, fine, tool0;
      MoveL Offs(PUNTO2, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v100, fine, tool0;
  ENDPROC
  PROC GoToP3()
      !func que va a maquina 3
      MoveJ Offs(PUNTO3, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v500, fine, tool0;
      MoveL Offs(PUNTO3, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}), v100, fine, tool0;
      MoveL Offs(PUNTO3, offset_PUNTO{1}, offset_PUNTO{2}, offset_PUNTO{3}+Zoffset), v100, fine, tool0;
  ENDPROC

ENDMODULE
