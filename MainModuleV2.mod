
 MODULE MainModule
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   CODI FASE 3 jdrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
    !
    !
    ! date
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !0) definicio i ini Vars

    !--------------------   CINEMATIC DEFINITIONS --------------------
    CONST robtarget HOME:=[[516.54,-11.81,715.94],[0.714516,0.0199544,0.699323,0.00399707],[-1,0,0,1],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget Ppalet1:=[[765.31,274.84,159.06],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget Ppalet2:=[[465.31,274.84,159.06],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO1:=[[-9.71,-458.23,282.62],[0.0487016,-0.732289,-0.678123,0.0391261],[-2,-1,0,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO2:=[[-66.12,488.43,69.72],[0.0545846,0.782406,-0.617839,0.0560026],[1,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PUNTO3:=[[-516.33,94.41,124.87],[0.00252886,0.999052,-0.0406592,0.015331],[1,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    CONST robtarget PDiscard:=[[20.31,274.84,159.06],[0.02082,-0.311179,-0.95011,0.00495981],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
    !Vars de posicio'
    CONST num offset_Ppalet{3} := [0,-10,-10];
    CONST num offset_PUNTO{3} := [-10, -10, -10];
    CONST num Zoffset := 50; !PUNT SEGURETAT VERTICAL
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

    VAR byte PosMosaic{4,15,3}; ![x,y,z], [pos], [mosaic] FIXME: revisar index posmosaic!<-------------------------------
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
    
    !------------ VARS "Privades" ------------
    VAR byte PvtAux1:=0; !func 1
    VAR byte PvtAux2:=0;


!1) Main func
PROC Main()
  TPErase;  !borrar pantalla
  GoToHome; !anar a Home pos

  ! Gestiona l entrada de dades de l usuari
  EnterNElements;
  CheckNElements;
  CmpPty;
  FillPosMosaic("CoolWay");

  !CONNECT PwrCinta1 WITH Trp_Cinta1; !FIXME: solve this
  !ISignalDI Palet1,1,PwrCinta1;

  !CONNECT PwrCinta2 WITH Trp_Cinta2;
  !ISignalDI Palet2,1,PwrCinta2;
  !activa 2 cintes

  WHILE Run DO
    WHILE (DetectCinta1 OR DetectCinta2) DO !mentres hi hagi algun palet
        GetMateria; !agafa la materia
        TPUI;
    ENDWHILE
    !mirar quin falta i demanar palet
    Run:=CountMateriaM{1}=materia{1} AND CountMateriaM{2}=materia{2} AND CountMateriaM{3}=materia{3};
    !Guarda a Run si ja ha omplert tot
  ENDWHILE
  PwrCinta1:=0;
  PwrCinta2:=0;
  !Apagar cintes i coses?
TPErase



ENDPROC


!2) Funcs aux

PROC CmpMachine(byte PvtCinta, num PvtIndex1)
!func que retorna el tipus de materia per trobar la estaico, chorras, FALTA ACABAr
  
  TEST Mosaic{PvtIndex1}
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



PROC FillPosMosaic(string s) !TODO: fer macu si tinc temps, Rta: no.
!Func que omple la matriu de posicions
    PosMosaic:=[[[10,10,2],[30,10,2],[20,25,2],[20,35,2],           [10,10,1],[30,10,1],[20,25,1],[20,35,1],          [10,10,0],[30,10,0],[20,25,0],[20,35,0],  [0,0,0],[0,0,0],[0,0,0]],
                [[10,10,2],[30,10,2],[10,30,2],[30,30,2],           [10,10,1],[30,10,1],[10,30,1],[30,30,1],          [10,10,0],[30,10,0],[10,30,0],[30,30,0],  [0,0,0],[0,0,0],[0,0,0]],
                [[5,15,2],[15,15,2],[25,15,2],[35,15,2],[20,35,2],  [5,15,1],[15,15,1],[25,15,1],[35,15,1],[20,35,1], [5,15,0],[15,15,0],[25,15,0],[35,15,0],[20,35,0]],
                [[5,15,2],[35,15,2],[25,35,2],[5,25,2],[20,20,2],   [5,15,1],[35,15,1],[25,35,1],[5,25,1],[20,20,1],  [5,15,0],[35,15,0],[25,35,0],[5,25,0],[20,20,0]]];
    
ENDPROC

PROC EnterNElements()
!Func per entrer els elements per materia
  VAR num resposta;
  VAR num i:=0;
  !FOR i FROM 1 TO 3 DO
  WHILE i < 4 DO
      TPErase;
      TPWrite "MAQUINA "\Num:=i;
      TPReadNum resposta,"Enter element count: ";
      IF resposta > 0 THEN
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
      TPWrite "MAQUINA "\Num:=i;
      TPReadNum resposta,"Enter priority level (form 1 to 3 being 1 the highest) pls: ";
      IF resposta > 0 AND resposta < 4 THEN
        priority{i}:=resposta;
      ELSE
        TPWrite "ENTER A VALID INPUT STUPID :( ";
        i:=i-1;
      ENDIF
    i:=i+1;
  ENDWHILE
  !ENDFOR

ENDPROC

PROC CheckNElements()
!Func per validar les dades entrades
  auxiliar{1}:=(Priority{1} * priority{2} * priority{3}); !hem fet el producte per veure si tenen nums de prty diferents i els adequats, i m a genius tnku
  WHILE( auxiliar{1} <> 6) DO 
    TPWrite "ENTER A VALID PRIORITY INPUT YOU BASTARD :(";
    EnterNElements;
    auxiliar{1}:=(Priority{1} * priority{2} * priority{3});
  ENDWHILE
    
ENDPROC

PROC GetPaletsToMachine(byte PvtCinta3, byte PvtIndex)
!num tipus,robtarget pallet
    !Flipa amb la recursivitat chavaless
    
    BackToZero(1);
    BackToZero(2);
    !Reset if needed
    TEST PvtCinta3 
    CASE 1:
        GoToPoint Ppalet1, PosMosaic{1, PvtIndex, Mosaic{PvtCinta3}}, PosMosaic{2, PvtIndex, Mosaic{PvtCinta3}}, PosMosaic{3, PvtIndex, Mosaic{PvtCinta3}}*10 ;
    CASE 2: 
        GoToPoint Ppalet2, PosMosaic{1, PvtIndex, Mosaic{PvtCinta3}}, PosMosaic{2, PvtIndex, Mosaic{PvtCinta3}}, PosMosaic{3, PvtIndex, Mosaic{PvtCinta3}}*10;
    ENDTEST
    !PROC CmpMachine(byte PvtCinta, num PvtIndex)
    PvtAux2:= CmpMachineOut;

    
    
    IF materia{PvtAux2} <> CountMateriaM{PvtAux2} THEN  
        PvtAux2:=4;
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



    IF PvtIndex <= SizeMosaic{Mosaic{PvtCinta3}} THEN
        Incr PvtIndex;
        GetPaletsToMachine PvtCinta3, PvtIndex;
    ELSEIF PvtCinta3 = 1 THEN
        !demanar palet 1
        
        !ISignalDI Palet1,1,PwrCinta1;
        RequestPalet{1}:=TRUE;
    ELSE
        !demanar palet 2
        
        !ISignalDI Palet2,1,PwrCinta2;
        RequestPalet{2}:=TRUE;
    ENDIF
!Anar a home mentres espera -> WaitReq

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

PROC WaitReq()
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


PROC CmpPty()
! Func que calcula la prioritat d una manera super pro
  ! aux 1->3
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
ENDPROC

PROC GetMateria()
!Here is where the magic happens btch winkwink 
  VAR byte Index:=1;
  VAR byte HighestPriority;
!TODO: gestio demanar palets si no n0hi han i tema de deixar un a mitges
  IF FlagPaletWithStock{1} AND FlagPaletWithStock{2} AND FlagNoStock = FALSE THEN !no funciona el NOT
    !HighestPriority:= StrToByte(ComputePriority{Index}); !TODO: solve this
    HighestPriority:= ComputePriority{2}; !TODO: solve this
  ELSEIF FlagPaletWithStock{1} THEN
    HighestPriority:= 1;
    !request 2
  ELSEIF FlagPaletWithStock{2} THEN
    HighestPriority:= 2;
    !request1
  ELSE
    WaitReq;
  ENDIF

  FOR j FROM 1 TO 2 DO !AQUI TENIM BYTE (NO POT SER BYTE; CAL ITERAR)
    TEST Mosaic{j}
    CASE 1:
      FOR k FROM 1 TO 4 DO
        IF HighestPriority = Mosaic1{k} THEN
          Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    CASE 2:
      FOR k FROM 1 TO 4 DO
        IF HighestPriority = Mosaic2{k} THEN
            Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    CASE 3:
      FOR k FROM 1 TO 4 DO
        IF HighestPriority = Mosaic3{k} THEN
            Incr auxiliar{j+1};
        ENDIF
      ENDFOR
    DEFAULT:
     FOR k FROM 1 TO 4 DO
        IF HighestPriority = Mosaic4{k} THEN
            Incr auxiliar{j+1};
        ENDIF
     ENDFOR
    ENDTEST
    

    !This is to find the cinta w more intresting parts (CASE BOTH 0 HANDELED BY THE FACT THAT THEY ARE EQUAL TO ZEROW)
  IF auxiliar{2} > auxiliar{3} THEN
    GetPaletsToMachine HighestPriority, 1;
    WaitReq;
  ELSE 
    GetPaletsToMachine HighestPriority, 1;
    WaitReq; !func que espera mentres no hi han palets
  
  ENDIF
ENDFOR
  
    !tot entrat i prioritat calculada
  
  !aqui sabrem que ha d agafar i per a quina maquina

ENDPROC


PROC TPUI()
!TPUI teach pendant User Interface PAT PENDING (R) TM
  TPErase;
  TPWrite " Suma peces total estaci? 1: "\Num:=materia{1};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{1};
  TPWrite " Falten per entrar: "\Num:=materia{1}-CountMateriaM{1};
  TPWrite " --------------------------------------";
  TPWrite " Suma peces total estaci? 2: "\Num:=materia{2};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{2};
  TPWrite " Falten per entrar: "\Num:=materia{2}-CountMateriaM{2};
  TPWrite " --------------------------------------";
  TPWrite " Suma peces total estaci? 3: "\Num:=materia{3};
  TPWrite " Peces entrades: "\Num:=CountMateriaM{3};
  TPWrite " Falten per entrar: "\Num:=materia{3}-CountMateriaM{3};
ENDPROC

PROC err()
!something went really wrong bro
  TPWrite "-- ERROR 418 I'm a teapot. Can t'make cofee :/ --";
  Stop;
ENDPROC

!Traps !TODO:SOLVE ARRAY?
  TRAP Trp_Cinta1 !neets update
    DetectCinta1:=TRUE;
    mosaic0:=0;
  ENDTRAP

  TRAP Trp_Cinta2
    DetectCinta2:=TRUE;
    mosaic1:=0;
  ENDTRAP !end missing

!Funcs aborrides de moviments a maquines i punts i tal
!{GoToPoint(punt, offset), GoToHome, GoToP1, GoToP2, GoToP3}
  PROC GoToPoint(robtarget PvtPTG, byte PvtOFST1, byte PvtOFST2, byte PvtOFST3) !found num, expected dim aixi que poso vars individuals i ja
    !INPUT: PointToGo
      !func que va qualsevol pos del palet de manera guai
      MoveJ Offs(PvtPTG, offset_Ppalet{1}+PvtOFST1, offset_Ppalet{2}+PvtOFST2, offset_Ppalet{3}+Zoffset+PvtOFST3), v500, fine, tool0;
      MoveL Offs(PvtPTG, offset_Ppalet{1}+PvtOFST1, offset_Ppalet{2}+PvtOFST2, offset_Ppalet{3}+PvtOFST3), v100, fine, tool0;
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
