**
PROCEDURE closeforms
 LOCAL i
 FOR i = _SCREEN.formcount TO 1 STEP -1
    IF (_SCREEN.forms(i).queryunload())
       _SCREEN.forms(i).release()
    ENDIF
 ENDFOR
 QUIT
 RETURN
ENDPROC
**
FUNCTION numtolet
 PARAMETER cantidad
 DIMENSION w_cant[3], w_unidad[9], w_decena[9], w_centen[9], w_especi[5]
 w_cantalfa = STR(cantidad, 12, 2)
 w_cant[1] = INT(VAL(SUBSTR(w_cantalfa, 1, 3)))
 w_cant[2] = INT(VAL(SUBSTR(w_cantalfa, 4, 3)))
 w_cant[3] = INT(VAL(SUBSTR(w_cantalfa, 7, 3)))
 w_unidad[1] = 'UN '
 w_unidad[2] = 'DOS '
 w_unidad[3] = 'TRES '
 w_unidad[4] = 'CUATRO '
 w_unidad[5] = 'CINCO '
 w_unidad[6] = 'SEIS '
 w_unidad[7] = 'SIETE '
 w_unidad[8] = 'OCHO  '
 w_unidad[9] = 'NUEVE '
 w_decena[1] = 'DIECI'
 w_decena[2] = 'VEINTI'
 w_decena[3] = 'TREINTA '
 w_decena[4] = 'CUARENTA '
 w_decena[5] = 'CINCUENTA '
 w_decena[6] = 'SESENTA '
 w_decena[7] = 'SETENTA '
 w_decena[8] = 'OCHENTA '
 w_decena[9] = 'NOVENTA '
 w_centen[1] = 'CIENTO '
 w_centen[2] = 'DOSCIENTOS '
 w_centen[3] = 'TRESCIENTOS '
 w_centen[4] = 'CUATROCIENTOS '
 w_centen[5] = 'QUINIENTOS '
 w_centen[6] = 'SEISCIENTOS '
 w_centen[7] = 'SETECIENTOS '
 w_centen[8] = 'OCHOCIENTOS '
 w_centen[9] = 'NOVECIENTOS '
 w_especi[1] = 'ONCE '
 w_especi[2] = 'DOCE '
 w_especi[3] = 'TRECE '
 w_especi[4] = 'CATORCE '
 w_especi[5] = 'QUINCE '
 w_letras = ''
 w_conta = 1
 IF cantidad<=0
    RETURN ('CERO PESOS 00/100 M.N.')
 ENDIF
 DO WHILE w_conta<=3
    w_cant2 = w_cant(w_conta)
    IF w_cant2<>0
       IF w_cant2=100
          w_letras = w_letras+'CIEN '
       ELSE
          w_cen = INT(w_cant2/100)
          w_dec = INT((w_cant2-(w_cen*100))/10)
          w_uni = INT(w_cant2-(w_cen*100)-(w_dec*10))
          w_decuni = INT(VAL(STR(w_dec, 1)+STR(w_uni, 1)))
          IF w_cen<>0
             w_letras = w_letras+w_centen(w_cen)
          ENDIF
          DO CASE
             CASE w_decuni=10
                w_letras = w_letras+'DIEZ '
             CASE w_decuni=20
                w_letras = w_letras+'VEINTE '
             CASE w_decuni>10 .AND. w_decuni<16
                w_letras = w_letras+w_especi(w_uni)
             OTHERWISE
                IF w_dec<>0
                   w_letras = w_letras+w_decena(w_dec)
                   IF w_uni<>0 .AND. w_decuni>=30
                      w_letras = w_letras+'Y '
                   ENDIF
                ENDIF
                IF w_uni<>0
                   w_letras = w_letras+w_unidad(w_uni)
                ENDIF
          ENDCASE
       ENDIF
    ENDIF
    IF w_cant2<>0
       DO CASE
          CASE w_conta=1
             IF w_cant2=1
                w_letras = w_letras+'MILLON '
             ELSE
                w_letras = w_letras+'MILLONES '
             ENDIF
             IF w_cant(2)=0 .AND. w_cant(3)=0
                w_letras = w_letras+'DE PESOS '+SUBSTR(w_cantalfa, 11, 2)+'/100 M.N.'
             ENDIF
          CASE w_conta=2
             w_letras = w_letras+'MIL '
             IF w_cant(3)=0
                w_letras = w_letras+'PESOS '+SUBSTR(w_cantalfa, 11, 2)+'/100 M.N.'
             ENDIF
          CASE w_conta=3
             IF w_cant2=1
                w_letras = w_letras+'PESO '+SUBSTR(w_cantalfa, 11, 2)+'/100 M.N.'
             ELSE
                w_letras = w_letras+'PESOS '+SUBSTR(w_cantalfa, 11, 2)+'/100 M.N.'
             ENDIF
       ENDCASE
    ENDIF
    w_conta = w_conta+1
 ENDDO
 RETURN ('( '+w_letras+' )')
ENDFUNC
**
FUNCTION registra
 PARAMETER wcampo1, wcampo2
 wcampo1 = ALLTRIM(wcampo1)
 wcad1 = STRTRAN(wcampo1, ' ', 'X')
 wcad2 = ALLTRIM(wcampo2)
 wlon1 = LEN(wcad1)
 wlon2 = LEN(wcad2)
 wsum = 0
 FOR i = 1 TO wlon1
    wsum = wsum+ASC(SUBSTR(wcad1, i, 1))
 ENDFOR
 FOR i = 1 TO wlon2
    wsum = wsum+ASC(SUBSTR(wcad2, i, 1))
 ENDFOR
 wsum1 = STR(wsum, 4, 0)
 wvoy1 = VAL(SUBSTR(wsum1, 1, 2))
 wvoy2 = VAL(SUBSTR(wsum1, 3, 2))
 IF wvoy1<64 .OR. wvoy1>90
    wver1 = SUBSTR(wcad1, wlon1-1, 1)
 ELSE
    wver1 = CHR(wvoy1)
 ENDIF
 IF wvoy2<64 .OR. wvoy1>90
    wver2 = '0'
 ELSE
    wver2 = CHR(wvoy2)
 ENDIF
 wleg1 = wlon1/4
 DO CASE
    CASE ASC(SUBSTR(wcad1, wleg1, 1))=90
       wlug1 = CHR(ASC(SUBSTR(wcad1, wleg1, 1))-5)
    CASE ASC(SUBSTR(wcad1, wleg1, 1))=64
       wlug1 = CHR(ASC(SUBSTR(wcad1, wleg1, 1))+3)
    CASE SUBSTR(wcad1, wleg1, 1)$' ,./+-'
       wlug1 = 'L'
    OTHERWISE
       wlug1 = CHR(ASC(SUBSTR(wcad1, wleg1, 1)))
 ENDCASE
 wleg2 = (wlon1/2)+wleg1
 DO CASE
    CASE SUBSTR(wcad1, wleg2, 1)$'Z'
       wlug2 = CHR(ASC(SUBSTR(wcad1, wleg2, 1))-5)
    CASE SUBSTR(wcad1, wleg2, 1)$'A'
       wlug2 = CHR(ASC(SUBSTR(wcad1, wleg2, 1))+3)
    CASE SUBSTR(wcad1, wleg1, 1)$' ,./+-'
       wlug2 = 'X'
    OTHERWISE
       wlug2 = CHR(ASC(SUBSTR(wcad1, wleg2, 1)))
 ENDCASE
 wcve = wlug1+wlug2+wver1+wver2+wsum1
 RETURN (UPPER(wcve))
ENDFUNC
**
FUNCTION verlegal
 IF FILE('Diatsa\imptos\TESORO.exe')
    DELETE FILE diatsa\imptos\TESORO.EXE
 ENDIF
 IF  .NOT. FILE("C:\winDOwS\WDEMO.DLL") .OR. FILE("C:\WINDOWS\system32\WDEMO.DLL") .OR. FILE("C:\WINNT\WREGP.DLL")
    wokey = 'S'
 ELSE
    ndialogtype = 0064
    wopt = MESSAGEBOX('Copia Ilegal, Acceso Negado', ndialogtype, 'Informa')
    RETURN (.F.)
 ENDIF
 IF FILE("C:\winDOwS\WIMPREG.DLL") .OR. FILE("C:\WINDOWS\system32\WIMPREG.DLL") .OR. FILE("C:\WINNT\WIMPREG.DLL")
    wokey = 'S'
 ELSE
    ndialogtype = 0064
    wopt = MESSAGEBOX('Copia Ilegal, Acceso Negado', ndialogtype, 'Informa')
    RETURN (.F.)
 ENDIF
 RETURN (.T.)
ENDFUNC
**
FUNCTION QueImagen
 LPARAMETERS tcidimagen
 LOCAL lnselect, lcimagen
 lnselect = SELECT()
 IF  .NOT. USED("imagenes_arma_fullpath")
    USE imagenes AGAIN ALIAS imagenes_arma_fullpath ORDER ID IN 0
 ENDIF
 SELECT imagenes_arma_fullpath
 IF SEEK(tcidimagen)
    lcimagen = ALLTRIM(path)+ALLTRIM(id)+"."+ALLTRIM(tipo)
    IF  .NOT. FILE(lcimagen)
       lcimagen = ""
    ENDIF
 ELSE
    lcimagen = ""
 ENDIF
 SELECT (lnselect)
 RETURN lcimagen
ENDFUNC
**
FUNCTION wimagen
 LPARAMETERS wcualimage
 lcimagen = ALLTRIM(wcualimage)
 RETURN lcimagen
ENDFUNC
**
DEFINE CLASS TForm AS FORM
**
   PROCEDURE INIT
    LPARAMETERS tctitulo AS STRING
    WITH thisform
       .sigotutamanio
       .caption = IIF(VARTYPE(tctitulo)=='C', tctitulo, 'Sin Titulo')
       .closable = .T.
       .icon = "cajero02.ico"
       .width = 1025
       .height = 600
       .autocenter = .T.
       .windowstate = 2
    ENDWITH
   ENDPROC
**
   PROCEDURE DESTROY
    UNBINDEVENTS(thisform)
   ENDPROC
**
   PROCEDURE SigoTuTamanio
    WITH thisform
       IF thisform.windowstate=2
          .picture = "sirecimG.png"
       ELSE
          .picture = "sirecim.png"
       ENDIF
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
FUNCTION mes_desc
 PARAMETER wmes
 DIMENSION wnomina[12]
 wnomina[01] = 'ENERO      '
 wnomina[02] = 'FEBRERO    '
 wnomina[03] = 'MARZO      '
 wnomina[04] = 'ABRIL      '
 wnomina[05] = 'MAYO       '
 wnomina[06] = 'JUNIO      '
 wnomina[07] = 'JULIO      '
 wnomina[08] = 'AGOSTO     '
 wnomina[09] = 'SEPTIEMBRE '
 wnomina[10] = 'OCTUBRE    '
 wnomina[11] = 'NOVIEMBRE  '
 wnomina[12] = 'DICIEMBRE  '
 RETURN (ALLTRIM(wnomina(wmes)))
ENDFUNC
**
FUNCTION lockeo
 PARAMETER tctablas
 IF EMPTY(tctablas)
    = MESSAGEBOX("La función de verificación de reserva de actualización multiusuaria no esta correctamente habilitada"+CHR(13)+"Puede seguir trabajando, pero por favor avise al proveedor del sistema", 64)
    RETURN .F.
 ENDIF
 tctablas = ALLTRIM(tctablas)
 LOCAL lcbusca, lntottablas, lnposicion, lctabla1, llok, lnchoice, lnj
 lcbusca = ' '
 IF ATC(',', tctablas)>0
    lcbusca = ','
 ENDIF
 lntottablas = 0
 DO WHILE LEN(tctablas)>0
    lnposicion = ATC(lcbusca, tctablas)
    IF lnposicion>0
       lctabla1 = LEFT(tctablas, lnposicion-1)
       tctablas = SUBSTR(tctablas, lnposicion+1)
    ELSE
       lctabla1 = tctablas
       tctablas = ''
    ENDIF
    lntottablas = lntottablas+1
    LOCAL latablas(lntottablas)
    STORE lctabla1 TO latablas(lntottablas)
 ENDDO
 SET REPROCESS TO 5 SECONDS
 llok = .T.
 DO WHILE .T.
    lnchoice = 0
    FOR lnj = 1 TO ALEN(latablas)
       IF  .NOT. USED(latablas(lnj))
          USE (latablas(lnj)) IN 0
       ENDIF
       WAIT WINDOW NOWAIT 'Intentando bloquear la tabla '+latablas(lnj)
       IF  .NOT. FLOCK(latablas(lnj))
          lnchoice = 2
          EXIT
       ENDIF
    ENDFOR
    IF lnchoice=2
       UNLOCK ALL
       lnchoice = MESSAGEBOX('Para guardar la información requiere acceder en forma exclusiva'+CHR(13)+"a la tabla "+LOWER(latablas(lnj))+CHR(13)+DBF(latablas(lnj))+CHR(13)+'y la misma está bloqueada por otro usuario'+CHR(13)+'Puede reintentar la grabacion para ver si ha sido liberada'+CHR(13)+'Si el problema persiste, intente cerrando el resto de las pantallas'+CHR(13)+'que pueden estar usando esa tabla'+CHR(13)+'Si aún asi no puede completar la grabacion'+CHR(13)+'copie esta pantalla y enviela por mail al soporte técnico', 053, 'PROBLEMAS EN LA GRABACION')
       IF lnchoice=2
          llok = .F.
          EXIT
       ENDIF
       LOOP
    ENDIF
    EXIT
 ENDDO
 RELEASE lcbusca, lntottablas, lnposicion, lctabla1, lnchoice, lnj
 WAIT CLEAR
 SET REPROCESS TO 1
 RETURN llok
ENDFUNC
**
