**
FUNCTION Print2PDF
 PARAMETER pcfilename, pcreport
 IF VARTYPE(pcfilename)<>"C" .OR. VARTYPE(pcreport)<>"C" .OR. EMPTY(pcfilename) .OR. EMPTY(pcreport)
    = MESSAGEBOX("No Parms passed to Print2PDF", 48, "Error")
    RETURN .F.
 ENDIF
 LOCAL lopdf
 lopdf = .NULL.
 lopdf = CREATEOBJECT("Print2PDF")
 IF ISNULL(lopdf)
    = MESSAGEBOX("Could not setup PDF class!", 48, "Error")
    RETURN .F.
 ENDIF
 WITH lopdf
    .coutputfile = pcfilename
    .creport = pcreport
    llresult = .main()
 ENDWITH
 IF  .NOT. llresult .OR. lopdf.lerror
    = MESSAGEBOX("This error ocurred creating the PDF:"+CHR(13)+ALLTRIM(lopdf.cerror), 48, "Error Message")
 ENDIF
 lopdf = .NULL.
 RELEASE lopdf
 RETURN .T.
ENDFUNC
**
DEFINE CLASS Print2PDF AS relation
 creport = SPACE(0)
 coutputfile = SPACE(0)
 cstartfolder = JUSTPATH(SYS(16))+IIF(RIGHT(JUSTPATH(SYS(16)), 1)="\", "", "\")
 ctemppath = SPACE(0)
 cextrarptclauses = SPACE(0)
 lreadini = .T.
 cinifile = this.cstartfolder+"Print2PDF.ini"
 lfoundprinter = .F.
 lfoundgs = .F.
 cpsprinter = SPACE(0)
 cpscolorprinter = SPACE(0)
 lusecolor = .F.
 cprintresolution = SPACE(0)
 cpsfile = SPACE(0)
 lerasepsfile = .T.
 cpdffile = SPACE(0)
 cgsfolder = SPACE(0)
 lerror = .F.
 cerror = ""
 corigsafety = SPACE(0)
 corigprinter = SPACE(0)
 iinstallcount = 1
 DIMENSION ainstall[1, 7]
 ainstall[1, 1] = SPACE(0)
 ainstall[1, 2] = .T.
 ainstall[1, 3] = SPACE(0)
 ainstall[1, 4] = SPACE(0)
 ainstall[1, 5] = SPACE(0)
 ainstall[1, 6] = SPACE(0)
 ainstall[1, 7] = SPACE(0)
**
   FUNCTION init
    LPARAMETERS pcfilename, pcreport, plrunnow
    WITH this
       .corigsafety = SET("safety")
       .corigprinter = SET("Printer", 3)
       .cstartfolder = FULLPATH(".")
       .cinifile = ADDBS(.cstartfolder)+"PRINT2PDF.INI"
       .lerror = .F.
       .cerror = ""
       IF TYPE("pcFileName")="C" .AND.  .NOT. EMPTY(pcfilename)
          .coutputfile = ALLTRIM(pcfilename)
       ENDIF
       IF TYPE("pcReport")="C" .AND.  .NOT. EMPTY(pcreport)
          .creport = ALLTRIM(pcreport)
       ENDIF
    ENDWITH
    SET SAFETY OFF
    IF TYPE("plRunNow")="L" .AND. plrunnow=.T.
       RETURN this.main()
    ENDIF
   ENDFUNC
**
   PROCEDURE CleanUp
    LOCAL lcorigprinter, lcorigsafety
    WITH this
       lcorigsafety = .corigsafety
       lcorigprinter = .corigprinter
    ENDWITH
    IF  .NOT. EMPTY(lcorigsafety)
       SET SAFETY &lcorigsafety
    ENDIF
    IF  .NOT. EMPTY(lcorigprinter)
       SET PRINTER TO
       SET PRINTER TO NAME "&lcOrigPrinter"
    ENDIF
    RETURN
   ENDPROC
**
   FUNCTION ResetError
    WITH this
       .lerror = .F.
       .ierror = 0
       .cerror = ""
    ENDWITH
    RETURN .T.
   ENDFUNC
**
   FUNCTION main
    LPARAMETERS pcfilename, pcreport
    LOCAL x
    STORE 0 TO x
    WITH this
       IF TYPE("pcFileName")="C" .AND.  .NOT. EMPTY(pcfilename)
          .coutputfile = ALLTRIM(pcfilename)
       ENDIF
       IF TYPE("pcReport")="C" .AND.  .NOT. EMPTY(pcreport)
          .creport = ALLTRIM(pcreport)
       ENDIF
       IF EMPTY(.creport) .OR. EMPTY(.coutputfile)
          .lerror = .T.
          .cerror(".cReport and/or .cOutputFile empty", 48, "Error")
          RETURN .F.
       ENDIF
       IF  .NOT. .lerror
          = .readini()
       ENDIF
       IF  .NOT. .lerror
          = .setprinter()
       ENDIF
       IF  .NOT. .lerror
          = .makeps()
       ENDIF
       IF  .NOT. .lerror
          = .gsfind()
       ENDIF
       IF  .NOT. .lerror
          = .makepdf()
       ENDIF
       IF  .NOT. .lerror
          = .instpdfreader()
       ENDIF
       .cleanup()
    ENDWITH
    RETURN  .NOT. this.lerror
   ENDFUNC
**
   FUNCTION ReadINI
    LOCAL lctmp
    STORE "" TO lctmp
    IF this.lreadini=.T.
       DECLARE INTEGER GetPrivateProfileString IN WIN32API STRING, STRING, STRING, STRING @, INTEGER, STRING
       WITH this
          IF EMPTY(.cpsprinter)
             .cpsprinter = .readinisetting("PostScript", "cPSPrinter")
          ENDIF
          IF EMPTY(.cpscolorprinter)
             .cpscolorprinter = .readinisetting("PostScript", "cPSColorPrinter")
          ENDIF
          IF EMPTY(.cpsfile)
             .cpsfile = .readinisetting("PostScript", "cPSFile")
          ENDIF
          IF EMPTY(.ctemppath)
             .ctemppath = .readinisetting("PostScript", "cTempPath")
          ENDIF
          IF EMPTY(.cpdffile)
             .cpdffile = .readinisetting("GhostScript", "cPDFFile")
          ENDIF
          IF EMPTY(.cgsfolder)
             .cgsfolder = .readinisetting("GhostScript", "cGSFolder")
          ENDIF
          IF EMPTY(.cprintresolution)
             .cprintresolution = .readinisetting("PostScript", "cPrintResolution")
          ENDIF
          lctmp = .readinisetting("Install", "iInstallCount")
          IF  .NOT. EMPTY(lctmp)
             .iinstallcount = VAL(lctmp)
             IF .iinstallcount>1
                DIMENSION .ainstall[.iinstallcount, 7]
             ENDIF
             FOR x = 1 TO .iinstallcount
                .ainstall[x, 1] = UPPER(.readinisetting("Install", "cInstID"+TRANSFORM(x)))
                lctmp = UPPER(.readinisetting("Install", "lAllowInst"+TRANSFORM(x)))
                .ainstall[x, 2] = IIF("T"$lctmp .OR. "Y"$lctmp, .T., .F.)
                .ainstall[x, 3] = .readinisetting("Install", "cInstProduct"+TRANSFORM(x))
                .ainstall[x, 4] = .readinisetting("Install", "cInstUserDescr"+TRANSFORM(x))
                .ainstall[x, 5] = .readinisetting("Install", "cInstFolder"+TRANSFORM(x))
                .ainstall[x, 6] = .readinisetting("Install", "cInstExe"+TRANSFORM(x))
                .ainstall[x, 7] = .readinisetting("Install", "cInstInstr"+TRANSFORM(x))
             ENDFOR
          ENDIF
       ENDWITH
    ENDIF
    IF EMPTY(.cpsprinter)
       .cpsprinter = "GENERIC POSTSCRIPT PRINTER"
    ENDIF
    IF EMPTY(.cpscolorprinter)
       .cpscolorprinter = "GENERIC COLOR POSTSCRIPT"
    ENDIF
    IF EMPTY(.ctemppath)
       .ctemppath = SYS(2023)+IIF(RIGHT(SYS(2023), 1)="\", "", "\")
    ENDIF
    IF EMPTY(.cprintresolution)
       .cprintresolution = "300"
    ENDIF
    RETURN .T.
   ENDFUNC
**
   FUNCTION ReadIniSetting
    LPARAMETERS pcsection, pcsetting
    LOCAL lcretvalue, lnnumret, lcfile
    lcfile = ALLTRIM(this.cinifile)
    lcretvalue = SPACE(8196)
    lnnumret = getprivateprofilestring(pcsection, pcsetting, "[MISSING]", @lcretvalue, 8196, lcfile)
    lcretvalue = ALLTRIM(SUBSTR(lcretvalue, 1, lnnumret))
    IF lcretvalue=="[MISSING]"
       lcretvalue = ""
    ENDIF
    RETURN lcretvalue
   ENDFUNC
**
   FUNCTION SetPrinter
    LOCAL x, lcprinter
    x = 0
    lcprinter = ""
    WITH this
       IF EMPTY(.cpsprinter)
          .cpsprinter = "GENERIC POSTSCRIPT PRINTER"
       ENDIF
       IF EMPTY(.cpscolorprinter)
          .cpscolorprinter = "GENERIC COLOR POSTSCRIPT"
       ENDIF
       IF .lusecolor=.T.
          lcprinter = .cpscolorprinter
       ELSE
          lcprinter = .cpsprinter
       ENDIF
       lcprinter = UPPER(lcprinter)
       .lfoundprinter = .F.
       IF APRINTERS(laprinters)>0
          FOR x = 1 TO ALEN(laprinters)
             IF ALLTRIM(UPPER(laprinters(x)))==lcprinter
                .lfoundprinter = .T.
             ENDIF
          ENDFOR
          IF  .NOT. .lfoundprinter
             .cerror = lcprinter+" is not installed!!"
             .lerror = .T.
          ENDIF
       ELSE
          .cerror = "NO printer drivers are installed!!"
          .lerror = .T.
       ENDIF
       IF .lfoundprinter
          lceval = "SET PRINTER TO NAME '"+lcprinter+"'"
          &lceval
          IF ALLTRIM(UPPER(SET("PRINTER", 3)))==ALLTRIM(UPPER(lcprinter))
          ELSE
             .cerror = "Could not set printer to: "+ALLTRIM(lcprinter)
             .lerror = .T.
             .lfoundprinter = .F.
          ENDIF
       ENDIF
       IF  .NOT. .lfoundprinter
          IF this.install("POSTSCRIPT")
             RETURN this.setprinter()
          ENDIF
       ENDIF
    ENDWITH
    RETURN .lfoundprinter
   ENDFUNC
**
   FUNCTION MakePS
    LOCAL lcreport, lcextra, lcpsfile
    SET SAFETY OFF
    WITH this
       IF  .NOT. .lfoundprinter
          IF  .NOT. .setprinter()
             RETURN .F.
          ENDIF
       ENDIF
       lcreport = .creport
       lcextra = .cextrarptclauses
       IF EMPTY(lcreport)
          .cerror = "No Report File was specified."
          .lerror = .T.
          RETURN .F.
       ENDIF
       IF EMPTY(.cpsfile)
          .cpsfile = .ctemppath+SYS(2015)+".ps"
       ENDIF
       lcpsfile = .cpsfile
       ERASE (lcpsfile)
       REPORT FORM (lcreport) &lcextra NOCONSOLE TO FILE &lcpsfile
       IF  .NOT. FILE(lcpsfile)
          .cerror = "Could create PDF file"
          .lerror = .T.
          RETURN .F.
       ENDIF
    ENDWITH
    RETURN .T.
   ENDFUNC
**
   FUNCTION GSFind
    LOCAL x, lcpath
    STORE "" TO lcpath
    STORE 0 TO x
    WITH this
       .lfoundgs = .F.
       IF FILE("gsdll32.dll")
          .lfoundgs = .T.
          RETURN .T.
       ENDIF
       IF  .NOT. EMPTY(.cgsfolder)
          lctmp = .cgsfolder+"gsdll32.dll"
          IF  .NOT. FILE(lctmp)
             .cgsfolder = ""
          ENDIF
       ENDIF
       IF EMPTY(.cgsfolder)
          IF  .NOT. DIRECTORY("C:\gs")
             RETURN .F.
          ENDIF
          ligs = ADIR(lagsfolders, "C:\gs\*.*", "D")
          IF ligs<1
             RETURN .F.
          ENDIF
          FOR x = 1 TO ALEN(lagsfolders, 1)
             lctmp = ALLTRIM(UPPER(lagsfolders(x, 1)))
             IF "GS"=LEFT(lctmp, 2) .AND. "D"$lagsfolders(x, 5)
                .cgsfolder = lctmp
                EXIT
             ENDIF
          ENDFOR
          IF EMPTY(.cgsfolder)
             RETURN .F.
          ENDIF
          .cgsfolder = "c:\gs\"+ALLTRIM(.cgsfolder)+"\bin\"
       ENDIF
       IF  .NOT. EMPTY(.cgsfolder)
          lctmp = .cgsfolder+"gsdll32.dll"
          IF  .NOT. FILE(lctmp)
             .cgsfolder = ""
          ENDIF
       ENDIF
       IF EMPTY(.cgsfolder)
          RETURN .F.
       ELSE
          .lfoundgs = .T.
       ENDIF
    ENDWITH
    lcpath = ALLTRIM(SET("Path"))
    SET PATH TO lcpath+";"+.cgsfolder
    RETURN .T.
   ENDFUNC
**
   FUNCTION MakePDF
    LOCAL lcpdffile, lcoutputfile, lcpsfile
    SET SAFETY OFF
    WITH this
       IF  .NOT. .lfoundgs
          IF  .NOT. .gsfind()
             IF .install("GHOSTSCRIPT")
                IF  .NOT. .gsfind()
                   .cerror = "Could not Install Ghostscript!"
                   .lerror = .T.
                   RETURN .F.
                ENDIF
             ENDIF
          ENDIF
       ENDIF
       lcoutputfile = .coutputfile
       lcpsfile = .cpsfile
       IF EMPTY(.cpdffile)
          .cpdffile = JUSTSTEM(lcpsfile)+".pdf"
       ENDIF
       lcpdffile = .cpdffile
       ERASE (lcpdffile)
       IF  .NOT. .gsconvertfile(lcpsfile, lcpdffile)
          .cerror = "Could not create: "+lcpdffile
          .lerror = .T.
       ENDIF
       IF  .NOT. FILE(lcpdffile)
          .cerror = "Could not create: "+lcpdffile
          .lerror = .T.
       ENDIF
       IF .lerasepsfile
          ERASE (lcpsfile)
       ENDIF
       ERASE (lcoutputfile)
       RENAME (lcpdffile) TO (lcoutputfile)
       IF  .NOT. FILE(lcoutputfile)
          .cerror = "Could not rename file to "+lcoutputfile
          .lerror = .T.
       ENDIF
    ENDWITH
    RETURN  .NOT. this.lerror
   ENDFUNC
**
   FUNCTION GSConvertFile
    LPARAMETERS tcfilein, tcfileout
    LOCAL lngsinstancehandle, lncallerhandle, loheap, lnelementcount, lcptrargs, lncounter, lnreturn
    DIMENSION laargs[11]
    STORE 0 TO lngsinstancehandle, lncallerhandle, lnelementcount, lncounter, lnreturn
    STORE .NULL. TO loheap
    STORE "" TO lcptrargs
    SET SAFETY OFF
    loheap = CREATEOBJECT('Heap')
    DECLARE LONG gsapi_new_instance IN gsdll32.dll LONG @, LONG
    DECLARE LONG gsapi_delete_instance IN gsdll32.dll LONG
    DECLARE LONG gsapi_init_with_args IN gsdll32.dll LONG, LONG, LONG
    DECLARE LONG gsapi_exit IN gsdll32.dll LONG
    laargs[1] = "dummy"
    laargs[2] = "-dNOPAUSE"
    laargs[3] = "-dBATCH"
    laargs[4] = "-dSAFER"
    laargs[5] = "-r"+this.cprintresolution
    laargs[6] = "-sDEVICE=pdfwrite"
    laargs[7] = "-sOutputFile="+tcfileout
    laargs[8] = "-c"
    laargs[9] = ".setpdfwrite"
    laargs[10] = "-f"
    laargs[11] = tcfilein
    lnreturn = gsapi_new_instance(@lngsinstancehandle, @lncallerhandle)
    IF (lnreturn<0)
       loheap = .NULL.
       RELEASE loheap
       this.lerror = .T.
       this.cerror = "Could not start Ghostscript."
       RETURN .F.
    ENDIF
    lnelementcount = ALEN(laargs)
    lcptrargs = ""
    FOR lncounter = 1 TO lnelementcount
       lcptrargs = lcptrargs+numtolong(loheap.allocstring(laargs(lncounter)))
    ENDFOR
    lnptr = loheap.allocblob(lcptrargs)
    lnreturn = gsapi_init_with_args(lngsinstancehandle, lnelementcount, lnptr)
    IF (lnreturn<0)
       loheap = .NULL.
       RELEASE loheap
       this.lerror = .T.
       this.cerror = "Could not Initilize Ghostscript."
       RETURN .F.
    ENDIF
    lnreturn = gsapi_exit(lngsinstancehandle)
    IF (lnreturn<0)
       loheap = .NULL.
       RELEASE loheap
       this.lerror = .T.
       this.cerror = "Could not Exit Ghostscript."
       RETURN .F.
    ENDIF
    = gsapi_delete_instance(lngsinstancehandle)
    loheap = .NULL.
    RELEASE loheap
    IF  .NOT. FILE(tcfileout)
       this.lerror = .T.
       this.cerror = "Ghostscript could not create the PDF."
       RETURN .F.
    ENDIF
    RETURN .T.
   ENDFUNC
**
   FUNCTION InstPDFReader
    IF  .NOT. FILE(.coutputfile)
       RETURN .F.
    ENDIF
    lcexe = .assocexe(.coutputfile)
    IF EMPTY(lcexe)
       RETURN .install("PDFREADER")
    ELSE
       RETURN .T.
    ENDIF
   ENDFUNC
**
   FUNCTION AssocExe
    LPARAMETERS pcfile
    LOCAL lcexefile
    STORE "" TO lcexefile
    DECLARE INTEGER FindExecutable IN shell32 STRING, STRING, STRING @
    lcexefile = SPACE(250)
    IF findexecutable(pcfile, "", @lcexefile)>32
       lcexefile = LEFT(lcexefile, AT(CHR(0), lcexefile)-1)
    ELSE
       lcexefile = ""
    ENDIF
    RETURN lcexefile
   ENDFUNC
**
   FUNCTION Install
    LPARAMETERS pcid
    LOCAL llfound, x, lceval, lcproduct, lcdesc, lctmp, lcfolder, lcinstexe, lcinstruct, lldynapath
    STORE "" TO lceval, lcproduct, lcabbr, lcdesc, lctmp, lcfolder, lcinstexe, lcinstruct
    STORE .F. TO llfound, lldynapath
    WITH this
       pcid = ALLTRIM(UPPER(pcid))
       FOR x = 1 TO ALEN(.ainstall, 1)
          IF ALLTRIM(UPPER(.ainstall(x, 1)))==pcid
             llfound = .T.
             EXIT
          ENDIF
       ENDFOR
       IF  .NOT. llfound
          .lerror = .T.
          .cerror = "Installation parms do not exist for ID: "+pcid
          RETURN .F.
       ENDIF
       lldoinst = .ainstall(x, 2)
       lcproduct = .ainstall(x, 3)
       lcdesc = .ainstall(x, 4)
       lcfolder = .ainstall(x, 5)
       lcinstexe = .ainstall(x, 6)
       IF  .NOT. EMPTY(.ainstall(x, 7))
          lcinstruct = ALLTRIM(.ainstall(x, 7))
          IF "+"$lcinstruct
             lcinstruct = &lcinstruct
          ENDIF
       ELSE
          lcinstruct = "Please accept the 'Default Values'"+CHR(13)+"during the installation."
       ENDIF
       IF "+"$lcfolder
          lldynapath = .T.
       ELSE
          lldynapath = .F.
       ENDIF
       IF lldoinst=.T.
          IF  .NOT. EMPTY(lcfolder) .AND.  .NOT. EMPTY(lcinstexe)
             IF lldynapath
                lcfolder = ALLTRIM(lcfolder)
                lceval = &lcfolder
                lceval = lceval+ALLTRIM(lcinstexe)
             ELSE
                IF RIGHT(lcfolder, 1)<>"\"
                   lcfolder = lcfolder+"\"
                ENDIF
                lceval = ALLTRIM(lcfolder)+ALLTRIM(lcinstexe)
             ENDIF
             IF  .NOT. lldynapath .AND.  .NOT. FILE(lceval)
                .cerror = "Could not find installer for "+lcproduct+" in:"+CHR(13)+ALLTRIM(lceval)
                .lerror = .T.
             ELSE
                IF 7=MESSAGEBOX(lcproduct+" needs to be installed on your computer."+CHR(13)+lcdesc+CHR(13)+"Is it OK to install now?", 36, "Confirmation")
                   .lerror = .T.
                   .cerror = "User cancelled "+lcproduct+" Installation."
                   RETURN .F.
                ENDIF
                = MESSAGEBOX(lcinstruct, 64, "Instructions")
                .ainstall[x, 2] = .F.
                lceval = "run /n "+lceval
                &lceval
                = MESSAGEBOX("When the Installation has finished"+CHR(13)+"COMPLETELY, please click OK...", 64, "Waiting for Installation...")
                IF 7=MESSAGEBOX("Was the installation successfull?"+CHR(13)+CHR(13)+"If no errors ocurred during the Installation"+CHR(13)+"and everything went OK, please click 'Yes'...", 36, "Everything OK?")
                   .lerror = .T.
                   .cerror = "Errors ocurred during "+lcproduct+" Installation."
                   RETURN .F.
                ELSE
                   .lerror = .F.
                   .cerror = ""
                   RETURN .T.
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ENDWITH
    RETURN .F.
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS heap AS custom
 PROTECTED inhandle, innumallocsactive, iaallocs[1, 3]
 inhandle = .NULL.
 innumallocsactive = 0
 iaallocs = .NULL.
 name = "heap"
**
   FUNCTION Alloc
    LPARAMETERS nsize
    DECLARE INTEGER HeapAlloc IN WIN32API AS HAlloc INTEGER, INTEGER, INTEGER
    DECLARE INTEGER HeapSize IN WIN32API AS HSize INTEGER, INTEGER, INTEGER
    LOCAL nptr
    WITH this
       nptr = halloc(.inhandle, 0, @nsize)
       IF nptr<>0
          .innumallocsactive = .innumallocsactive+1
          DIMENSION .iaallocs[.innumallocsactive, 3]
          .iaallocs[.innumallocsactive, 1] = nptr
          .iaallocs[.innumallocsactive, 2] = hsize(.inhandle, 0, nptr)
          .iaallocs[.innumallocsactive, 3] = .T.
       ELSE
          nptr = .NULL.
       ENDIF
    ENDWITH
    RETURN nptr
   ENDFUNC
**
   FUNCTION AllocBLOB
    LPARAMETERS cbstringtocopy
    LOCAL nallocptr
    WITH this
       nallocptr = .alloc(LEN(cbstringtocopy))
       IF  .NOT. ISNULL(nallocptr)
          .copyto(nallocptr, cbstringtocopy)
       ENDIF
    ENDWITH
    RETURN nallocptr
   ENDFUNC
**
   FUNCTION AllocString
    LPARAMETERS cstring
    RETURN this.allocblob(cstring+CHR(0))
   ENDFUNC
**
   FUNCTION AllocInitAs
    LPARAMETERS nsizeofbuffer, nbytevalue
    IF TYPE('nByteValue')<>'N' .OR.  .NOT. BETWEEN(nbytevalue, 0, 255)
       nbytevalue = 0
    ENDIF
    RETURN this.allocblob(REPLICATE(CHR(nbytevalue), nsizeofbuffer))
   ENDFUNC
**
   FUNCTION DeAlloc
    LPARAMETERS nptr
    DECLARE INTEGER HeapFree IN WIN32API AS HFree INTEGER, INTEGER, INTEGER
    LOCAL nctr
    nctr = .NULL.
    WITH this
       nctr = .findallocid(nptr)
       IF  .NOT. ISNULL(nctr)
          = hfree(.inhandle, 0, nptr)
          .iaallocs[nctr, 3] = .F.
       ENDIF
    ENDWITH
    RETURN  .NOT. ISNULL(nctr)
   ENDFUNC
**
   FUNCTION CopyTo
    LPARAMETERS nptr, csource
    DECLARE RtlMoveMemory IN WIN32API AS RtlCopy INTEGER, STRING @, INTEGER
    LOCAL nctr
    nctr = .NULL.
    IF TYPE('nPtr')='N' .AND. TYPE('cSource')$'CM' .AND.  .NOT. (ISNULL(nptr) .OR. ISNULL(csource))
       WITH this
          nctr = .findallocid(nptr)
          IF  .NOT. ISNULL(nctr)
             = rtlcopy((.iaallocs(nctr, 1)), csource, MIN(LEN(csource), .iaallocs(nctr, 2)))
          ENDIF
       ENDWITH
    ENDIF
    RETURN  .NOT. ISNULL(nctr)
   ENDFUNC
**
   FUNCTION CopyFrom
    LPARAMETERS nptr
    DECLARE RtlMoveMemory IN WIN32API AS RtlCopy STRING @, INTEGER, INTEGER
    LOCAL nctr, ubuffer
    ubuffer = .NULL.
    nctr = .NULL.
    IF TYPE('nPtr')='N' .AND.  .NOT. ISNULL(nptr)
       WITH this
          nctr = .findallocid(nptr)
          IF  .NOT. ISNULL(nctr)
             ubuffer = REPLICATE(CHR(0), .iaallocs(nctr, 2))
             = rtlcopy(@ubuffer, (.iaallocs(nctr, 1)), (.iaallocs(nctr, 2)))
          ENDIF
       ENDWITH
    ENDIF
    RETURN ubuffer
   ENDFUNC
**
   PROTECTED FUNCTION FindAllocID
    LPARAMETERS nptr
    LOCAL nctr
    WITH this
       FOR nctr = 1 TO .innumallocsactive
          IF .iaallocs(nctr, 1)=nptr .AND. .iaallocs(nctr, 3)
             EXIT
          ENDIF
       ENDFOR
       RETURN IIF(nctr<=.innumallocsactive, nctr, .NULL.)
    ENDWITH
   ENDFUNC
**
   FUNCTION SizeOfBlock
    LPARAMETERS nptr
    LOCAL nctr, nsizeofblock
    nsizeofblock = .NULL.
    WITH this
       nctr = .findallocid(nptr)
       RETURN IIF(ISNULL(nctr), .NULL., .iaallocs(nctr, 2))
    ENDWITH
   ENDFUNC
**
   PROCEDURE Destroy
    DECLARE HeapDestroy IN WIN32API AS HDestroy INTEGER
    LOCAL nctr
    WITH this
       FOR nctr = 1 TO .innumallocsactive
          IF .iaallocs(nctr, 3)
             .dealloc(.iaallocs(nctr, 1))
          ENDIF
       ENDFOR
       hdestroy(.inhandle)
    ENDWITH
    DODEFAULT()
   ENDPROC
**
   FUNCTION Init
    DECLARE INTEGER HeapCreate IN WIN32API AS HCreate INTEGER, INTEGER, INTEGER
    WITH this
       .inhandle = hcreate(0, 008192, 0)
       DIMENSION .iaallocs[1, 3]
       .iaallocs[1, 1] = 0
       .iaallocs[1, 2] = 0
       .iaallocs[1, 3] = .F.
       .innumallocsactive = 0
    ENDWITH
    RETURN (this.inhandle<>0)
   ENDFUNC
**
ENDDEFINE
**
FUNCTION SetMem
 LPARAMETERS nptr, csource
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopy INTEGER, STRING @, INTEGER
 rtlcopy(nptr, csource, LEN(csource))
 RETURN .T.
ENDFUNC
**
FUNCTION GetMem
 LPARAMETERS nptr, nlen
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopy STRING @, INTEGER, INTEGER
 LOCAL ubuffer
 ubuffer = REPLICATE(CHR(0), nlen)
 = rtlcopy(@ubuffer, nptr, nlen)
 RETURN ubuffer
ENDFUNC
**
FUNCTION GetMemString
 LPARAMETERS nptr, nsize
 DECLARE INTEGER lstrcpyn IN WIN32API AS StrCpyN STRING @, INTEGER, INTEGER
 LOCAL ubuffer
 IF TYPE('nSize')<>'N' .OR. ISNULL(nsize)
    nsize = 512
 ENDIF
 ubuffer = REPLICATE(CHR(0), nsize)
 IF strcpyn(@ubuffer, nptr, nsize-1)<>0
    ubuffer = LEFT(ubuffer, MAX(0, AT(CHR(0), ubuffer)-1))
 ELSE
    ubuffer = .NULL.
 ENDIF
 RETURN ubuffer
ENDFUNC
**
FUNCTION SHORTToNum
 LPARAMETERS tcint
 LOCAL b0, b1, nretval
 b0 = ASC(tcint)
 b1 = ASC(SUBSTR(tcint, 2, 1))
 IF b1<128
    nretval = b1*256+b0
 ELSE
    b1 = 255-b1
    b0 = 256-b0
    nretval = -((b1*256)+b0)
 ENDIF
 RETURN nretval
ENDFUNC
**
FUNCTION NumToSHORT
 LPARAMETERS tnnum
 LOCAL b0, b1, x
 IF tnnum>=0
    x = INT(tnnum)
    b1 = INT(x/256)
    b0 = MOD(x, 256)
 ELSE
    x = INT(-tnnum)
    b1 = 255-INT(x/256)
    b0 = 256-MOD(x, 256)
    IF b0=256
       b0 = 0
       b1 = b1+1
    ENDIF
 ENDIF
 RETURN CHR(b0)+CHR(b1)
ENDFUNC
**
FUNCTION DWORDToNum
 LPARAMETERS tcdword
 LOCAL b0, b1, b2, b3
 b0 = ASC(tcdword)
 b1 = ASC(SUBSTR(tcdword, 2, 1))
 b2 = ASC(SUBSTR(tcdword, 3, 1))
 b3 = ASC(SUBSTR(tcdword, 4, 1))
 RETURN (((b3*256+b2)*256+b1)*256+b0)
ENDFUNC
**
FUNCTION NumToDWORD
 LPARAMETERS tnnum
 RETURN numtolong(tnnum)
ENDFUNC
**
FUNCTION WORDToNum
 LPARAMETERS tcword
 RETURN (256*ASC(SUBSTR(tcword, 2, 1)))+ASC(tcword)
ENDFUNC
**
FUNCTION NumToWORD
 LPARAMETERS tnnum
 LOCAL x
 x = INT(tnnum)
 RETURN CHR(MOD(x, 256))+CHR(INT(x/256))
ENDFUNC
**
FUNCTION NumToLong
 LPARAMETERS tnnum
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopyLong STRING @, INTEGER @, INTEGER
 LOCAL cstring
 cstring = SPACE(4)
 = rtlcopylong(@cstring, BITOR(tnnum, 0), 4)
 RETURN cstring
ENDFUNC
**
FUNCTION LongToNum
 LPARAMETERS tclong
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopyLong INTEGER @, STRING @, INTEGER
 LOCAL nnum
 nnum = 0
 = rtlcopylong(@nnum, tclong, 4)
 RETURN nnum
ENDFUNC
**
FUNCTION AllocNetAPIBuffer
 LPARAMETERS nsize
 IF TYPE('nSize')<>'N' .OR. nsize<=0
    RETURN .NULL.
 ENDIF
 IF  .NOT. 'NT'$OS()
    RETURN .NULL.
 ENDIF
 DECLARE INTEGER NetApiBufferAllocate IN NETAPI32.DLL INTEGER, INTEGER
 LOCAL nbufferpointer
 nbufferpointer = 0
 IF netapibufferallocate(INT(nsize), @nbufferpointer)<>0
    nbufferpointer = .NULL.
 ENDIF
 RETURN nbufferpointer
ENDFUNC
**
FUNCTION DeAllocNetAPIBuffer
 LPARAMETERS nptr
 IF TYPE('nPtr')<>'N'
    RETURN .F.
 ENDIF
 IF  .NOT. 'NT'$OS()
    RETURN .F.
 ENDIF
 DECLARE INTEGER NetApiBufferFree IN NETAPI32.DLL INTEGER
 RETURN (netapibufferfree(INT(nptr))=0)
ENDFUNC
**
FUNCTION CopyDoubleToString
 LPARAMETERS ndoubletocopy
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopyDbl STRING @, DOUBLE @, INTEGER
 LOCAL cstring
 cstring = SPACE(8)
 = rtlcopydbl(@cstring, ndoubletocopy, 8)
 RETURN cstring
ENDFUNC
**
FUNCTION DoubleToNum
 LPARAMETERS cdoubleinstring
 DECLARE RtlMoveMemory IN WIN32API AS RtlCopyDbl DOUBLE @, STRING @, INTEGER
 LOCAL nnum
 nnum = 0.000000000000000000 
 = rtlcopydbl(@nnum, cdoubleinstring, 8)
 RETURN nnum
ENDFUNC
**
