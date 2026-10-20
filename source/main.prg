*** 
*** ReFox XI+  #OM124849  Darth-Mind  EMBRACE [VFP90]
***
 PUBLIC wregisx
 IF WEXIST("Standard")
    HIDE WINDOW "Standard"
 ENDIF
 IF WEXIST("Estándar")
    HIDE WINDOW "Estándar"
 ENDIF
 SET PROCEDURE TO z:\sirecim\source\Utils, z:\sirecim\source\gencfdi, z:\Tools\print2pdf, z:\Tools\CancelaCfdi, z:\tools\procs
 SET TALK OFF
 SET STATUS OFF
 SET SAFETY OFF
 SET STATUS BAR OFF
 SET DELETED ON
 SET SEPARATOR TO ','
 SET POINT TO '.'
 SET DATE french
 SET EXCLUSIVE OFF
 SET SYSMENU OFF
 CLEAR SCREEN
 USE SareDb\kontrol
 _SCREEN.picture = "sirecim.png"
 _SCREEN.closable = .T.
 _SCREEN.caption = "Recaudación Municipal - "+kontrol.nombrecont
 _SCREEN.icon = "Cajero02.ico"
 _SCREEN.width = 1025
 _SCREEN.height = 600
 _SCREEN.autocenter = .T.
 _SCREEN.windowstate = 0
 wtitsis = kontrol.nombrecont
 wsersis = kontrol.serie
 wdiract = FULLPATH(CURDIR())
 wruta = FULLPATH(CURDIR())
 wruta = wruta+"Listados"
 USE
 SET PATH TO catsat ADDITIVE 
 ON SHUTDOWN DO closeforms
 CLEAR SCREEN
 wquionda = .T.
 IF  .NOT. wquionda
    CLOSE DATABASES
    SET SYSMENU TO DEFAULT
    ON SHUTDOWN
    RETURN
 ENDIF
 wregisx = registra(wtitsis, wsersis)
 DO FOXYPREVIEWER.APP
 DO FORM LOGIN
 DO CASE
    CASE _SCREEN.login=0
       CLOSE DATABASES
       SET SYSMENU TO DEFAULT
       ON SHUTDOWN
       RETURN
    CASE _SCREEN.login=1
       CLOSE DATABASES
       SET SYSMENU TO DEFAULT
       ON SHUTDOWN
       RETURN
    CASE _SCREEN.login=4
       DO Imptmnu.mpr
       READ EVENTS
       CLOSE DATABASES
       SET SYSMENU TO DEFAULT
       ON SHUTDOWN
       RETURN
 ENDCASE
 RETURN
ENDPROC
**
*** 
*** ReFox - todo no se pierde 
***
