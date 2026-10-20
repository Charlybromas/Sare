**
PROCEDURE CFDInit
 PUBLIC cfdversions, cfdconf
 cfdversions = CREATEOBJECT("CFDVersionsEnum")
 cfdconf = CREATEOBJECT("CFDConf")
 DECLARE Sleep IN kernel32 INTEGER
 DECLARE INTEGER GetShortPathName IN kernel32 STRING, STRING @, INTEGER
 DECLARE INTEGER GenerateFile IN BarCodeLibrary.dll STRING, STRING
ENDPROC
**
DEFINE CLASS CFDVersionsEnum AS Custom
 cfd_20 = 2
 cfd_22 = 22
 cfdi_30 = 3
 cfdi_32 = 32
 cfdi_33 = 33
 validversionlist = "[2][22][3][32][33]"
**
   FUNCTION ToString
    LPARAMETERS pnversion
    DO CASE
       CASE pnversion=this.cfd_20
          RETURN "2.0"
       CASE pnversion=this.cfd_22
          RETURN "2.2"
       CASE pnversion=this.cfdi_30
          RETURN "3.0"
       CASE pnversion=this.cfdi_32
          RETURN "3.2"
       CASE pnversion=this.cfdi_33
          RETURN "3.3"
       OTHERWISE
          RETURN ""
    ENDCASE
   ENDFUNC
**
   FUNCTION FromString
    LPARAMETERS pcversion
    DO CASE
       CASE pcversion="2.0"
          RETURN this.cfd_20
       CASE pcversion="2.2"
          RETURN this.cfd_22
       CASE pcversion="3.0"
          RETURN this.cfdi_30
       CASE pcversion="3.2"
          RETURN this.cfdi_32
       CASE pcversion="3.3"
          RETURN this.cfdi_33
       OTHERWISE
          RETURN 0
    ENDCASE
   ENDFUNC
**
   FUNCTION ToLongString
    LPARAMETERS pcversion
    DO CASE
       CASE pcversion=this.cfd_20
          RETURN "CFD 2.0"
       CASE pcversion=this.cfd_22
          RETURN "CFD 2.2"
       CASE pcversion=this.cfdi_30
          RETURN "CFDi 3.0"
       CASE pcversion=this.cfdi_32
          RETURN "CFDi 3.2"
       CASE pcversion=this.cfdi_33
          RETURN "CFDi 3.3"
       OTHERWISE
          RETURN "Valor incorrecto ("+pcversion+")"
    ENDCASE
   ENDFUNC
**
   FUNCTION Validate
    LPARAMETERS vnewval, pucurvalue
    LOCAL uvalue
    IF VARTYPE(m.vnewval)="N"
       m.vnewval = cfdversions.tostring(m.vnewval)
    ENDIF
    uvalue = cfdversions.fromstring(m.vnewval)
    IF EMPTY(uvalue)
       uvalue = pucurvalue
       MESSAGEBOX("El valor de version indicado no es correcto.", 16, "CFD")
    ENDIF
    RETURN uvalue
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDConf AS Custom
 version = "3.7"
 openssl = ""
 smtpserver = ""
 smtpport = ""
 smtpusessl = .T.
 smtpauthenticate = .T.
 smtpusername = ""
 smtppassword = ""
 mailsender = ""
 ultimoerror = ""
 modopruebas = .F.
 metododigest = "md5"
 ubicacionrepositorio = ".\CFD"
 ultimocertificado = .NULL.
 formatoimpresion = "CFDI33.FRX"
 xmlversion = 0
 incluirbom = .F.
 ntocmoneda = "Pesos"
 ntocprefijo = ""
 ntocsufijo = "M.N."
 usarprint2pdf = .T.
**
   PROCEDURE Init
    this.version = cfdversions.cfdi_33
    this.openssl = ADDBS(FULLPATH(".\SSL"))
    this.xmlversion = cfdversions.cfdi_33
   ENDPROC
**
   PROCEDURE Version_Assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   PROCEDURE XMLVersion_Assign
    LPARAMETERS vnewval
    this.xmlversion = cfdversions.validate(m.vnewval, this.xmlversion)
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDComprobante AS Custom
 version = ""
 folio = 0
 fecha = {}
 sello = ""
 formapago = ""
 noaprobacion = 0
 anoaprobacion = 0
 nocertificado = ""
 subtotal = 0.00 
 total = 0.00 
 tipodecomprobante = ""
 emisor = .NULL.
 receptor = .NULL.
 conceptos = .NULL.
 serie = ""
 certificado = ""
 condicionesdepago = ""
 descuento = 0.00 
 motivodescuento = ""
 impuestos = .NULL.
 addenda = .NULL.
 cfdirelacionados = .NULL.
 montofoliofiscalorig = .NULL.
 fechafoliofiscalorig = {}
 seriefoliofiscalorig = .NULL.
 foliofiscalorig = .NULL.
 numctapago = .NULL.
 moneda = .NULL.
 tipocambio = .NULL.
 lugarexpedicion = .NULL.
 metodopago = ""
 cadenaoriginal = ""
 ubicacionopenssl = ".\SSL"
**
   FUNCTION cadenaOriginal_Access
    RETURN this._gencadenaoriginal()
   ENDFUNC
**
   PROCEDURE cadenaOriginal_Assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   PROCEDURE Init
    LPARAMETERS pnversion
    IF PCOUNT()=0
       pnversion = cfdconf.xmlversion
    ENDIF
    this.version = pnversion
    this.emisor = CREATEOBJECT("CFDPersona")
    this.receptor = CREATEOBJECT("CFDPersona")
    this.cfdirelacionados = CREATEOBJECT("CfdiRelacionados")
    this.conceptos = CREATEOBJECT("CFDConceptos")
    this.impuestos = CREATEOBJECT("CFDImpuestos")
    this.ubicacionopenssl = cfdconf.openssl
   ENDPROC
**
   PROCEDURE Version_Assign
    LPARAMETERS vnewval
    LOCAL nversion
    nversion = cfdversions.validate(m.vnewval, 0)
    IF nversion>0
       this.version = cfdversions.tostring(nversion)
    ENDIF
   ENDPROC
**
   FUNCTION _genCadenaOriginal
    LOCAL ctempfile
    ctempfile = gettempfile("XML")
    this.crearxml(ctempfile)
    LOCAL cstr
    cstr = cfdextraercadenaoriginal(ctempfile)
    ERASE (ctempfile)
    RETURN cstr
   ENDFUNC
**
   HIDDEN FUNCTION _fixStr
    LPARAMETERS puvalue, pcsep
    IF PCOUNT()=1
       pcsep = ""
    ENDIF
    LOCAL ctype
    ctype = VARTYPE(puvalue)
    DO CASE
       CASE ctype="N"
          IF EMPTY(puvalue)
             RETURN ""
          ENDIF
          RETURN ALLTRIM(STR(puvalue, 15, 2))+pcsep
       CASE ctype="D"
          IF EMPTY(puvalue)
             RETURN ""
          ENDIF
          RETURN STR(YEAR(puvalue), 4)+"-"+PADL(MONTH(puvalue), 2, "0")+"-"+PADL(DAY(puvalue), 2, "0")+pcsep
       CASE ctype="T"
          IF EMPTY(puvalue)
             RETURN ""
          ENDIF
          RETURN STR(YEAR(puvalue), 4)+"-"+PADL(MONTH(puvalue), 2, "0")+"-"+PADL(DAY(puvalue), 2, "0")+"T"+PADL(HOUR(puvalue), 2, "0")+":"+PADL(MINUTE(puvalue), 2, "0")+":"+PADL(SEC(puvalue), 2, "0")+pcsep
       CASE ctype="X"
          RETURN ""
    ENDCASE
    LOCAL cfixed
    cfixed = ALLTRIM(puvalue)
    cfixed = STRT(STRT(STRT(STRT(STRT(cfixed,[&],[&amp;]),[<],[&lt;]),[>],[&gt;]),["],[&quot;]),['],[&apos;])
    cfixed = STRTRAN(cfixed, CHR(13)+CHR(10), "")
    DO WHILE AT(SPACE(2), cfixed)<>0
       cfixed = STRTRAN(cfixed, SPACE(2), SPACE(1))
    ENDDO
    IF LEN(cfixed)>0 .AND.  .NOT. EMPTY(pcsep)
       cfixed = cfixed+pcsep
    ENDIF
    RETURN cfixed
   ENDFUNC
**
   FUNCTION Sellar
    LPARAMETERS pcarchivokey, pcpassword
    cfdconf.ultimoerror = ""
    LOCAL cmetodo
    cmetodo = "md5"
    IF YEAR(this.fecha)>2010
       cmetodo = "sha256"
    ENDIF
    cfdconf.metododigest = cmetodo
    LOCAL ccadenaoriginal
    ccadenaoriginal = this.cadenaoriginal
    IF EMPTY(ccadenaoriginal)
       cfdconf.ultimoerror = "No se pudo obtener la cadena original. Utilize CFDProbarOpenSSL() para verificar el funcionamiento de la libreria OpenSSL"
       RETURN .F.
    ENDIF
    this.sello = cfdgenerarsello(ccadenaoriginal, pcarchivokey, pcpassword, cmetodo, this.ubicacionopenssl)
    RETURN  .NOT. EMPTY(this.sello)
   ENDFUNC
**
   FUNCTION leerCertificado
    LPARAMETERS pcarchivocer
    LOCAL ocert
    IF ISNULL(cfdconf.ultimocertificado) .OR. cfdconf.ultimocertificado.archivo<>LOWER(pcarchivocer)
       ocert = cfdleercertificado(pcarchivocer)
       IF ISNULL(ocert)
          RETURN .NULL.
       ENDIF
       cfdconf.ultimocertificado = ocert
    ELSE
       ocert = cfdconf.ultimocertificado
    ENDIF
    IF ocert.valido .AND. (ocert.vigente .OR. cfdconf.modopruebas)
       this.certificado = ocert.certificado
       this.nocertificado = ocert.serial
    ENDIF
    RETURN ocert
   ENDFUNC
**
   FUNCTION CrearXML
    LPARAMETERS pcarchivo, plvalidar, pcarchivokey, pcpassword, pcmetodo
    cfdconf.ultimoerror = ""
    LOCAL oparser, nversion
    oparser = CREATEOBJECT("XmlParser")
    nversion = cfdversions.fromstring(this.version)
    WITH oparser
       .indentstring = ""
       .new()
       .xml.addnode("Comprobante")
       IF INLIST(nversion, cfdversions.cfdi_32, cfdversions.cfdi_33)
          .xml._comprobante.namespace = "cfdi"
       ENDIF
       WITH .xml._comprobante
          DO CASE
             CASE nversion=cfdversions.cfd_20
                .addprop("xmlns", "http://www.sat.gob.mx/cfd/2")
                .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/2 http://www.sat.gob.mx/sitio_internet/cfd/2/cfdv2.xsd")
             CASE nversion=cfdversions.cfdi_30
                .addprop("xmlns:cfdi", "http://www.sat.gob.mx/cfd/3")
                .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv3.xsd")
             CASE nversion=cfdversions.cfd_22
                .addprop("xmlns", "http://www.sat.gob.mx/cfd/2")
                .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/2 http://www.sat.gob.mx/sitio_internet/cfd/2/cfdv22.xsd")
             CASE nversion=cfdversions.cfdi_32
                .addprop("xmlns:cfdi", "http://www.sat.gob.mx/cfd/3")
                .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv32.xsd")
             CASE nversion=cfdversions.cfdi_33
                IF this.tipodecomprobante='P'
                   .addprop("xmlns:cfdi", "http://www.sat.gob.mx/cfd/3")
                   .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                   .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd")
                   .addprop("xmlns:pago10", "http://www.sat.gob.mx/Pagos")
                   .addprop("xmlns:xs", "http://www.w3.org/2001/XMLSchema")
                ELSE
                   IF this.tipodecomprobante='N'
                      .addprop("xmlns:cfdi", "http://www.sat.gob.mx/cfd/3")
                      .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                      .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd http://www.sat.gob.mx/nomina12 http://www.sat.gob.mx/sitio_internet/cfd/nomina/nomina12.xsd")
                      .addprop("xmlns:nomina12", "http://www.sat.gob.mx/nomina12")
                   ELSE
                      .addprop("xmlns:cfdi", "http://www.sat.gob.mx/cfd/3")
                      .addprop("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                      .addprop("xsi:schemaLocation", "http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd")
                   ENDIF
                ENDIF
          ENDCASE
          .addprop("Version", this._fixstr(this.version))
          .addprop("Serie", this._fixstr(this.serie), .T.)
          .addprop("Folio", this._fixstr(STR(this.folio, 10, 0)))
          .addprop("Fecha", this._fixstr(this.fecha))
          IF INLIST(nversion, cfdversions.cfd_20, cfdversions.cfd_22)
             .addprop("noAprobacion", this._fixstr(STR(this.noaprobacion, 10, 0)))
             .addprop("anoAprobacion", this._fixstr(STR(this.anoaprobacion, 10, 0)))
          ENDIF
          IF INLIST(nversion, cfdversions.cfd_22, cfdversions.cfdi_32, cfdversions.cfdi_33)
             .addprop("Moneda", this._fixstr(this.moneda), .T.)
             .addprop("TipoCambio", this._fixstr(this.tipocambio), .T.)
          ENDIF
          IF INLIST(nversion, cfdversions.cfdi_33)
             .addprop("MontoFolioFiscalOrig", this._fixstr(this.montofoliofiscalorig), .T.)
             .addprop("FechaFolioFiscalOrig", this._fixstr(this.fechafoliofiscalorig), .T.)
             .addprop("SerieFolioFiscalOrig", this._fixstr(this.seriefoliofiscalorig), .T.)
             .addprop("FolioFiscalOrig", this._fixstr(this.foliofiscalorig), .T.)
             .addprop("NumCtaPago", this._fixstr(this.numctapago), .T.)
             .addprop("LugarExpedicion", this._fixstr(this.lugarexpedicion))
          ENDIF
          .addprop("MetodoPago", this._fixstr(this.metodopago), .T.)
          IF this.tipodecomprobante='I'
             .addprop("FormaPago", this._fixstr(this.formapago))
          ENDIF
          .addprop("CondicionesDePago", this._fixstr(this.condicionesdepago), .T.)
          IF this.tipodecomprobante='I' .OR. this.tipodecomprobante='N'
             .addprop("SubTotal", this._fixstr(STR(this.subtotal, 15, 2)))
          ELSE
             .addprop("SubTotal", this._fixstr('0'))
          ENDIF
          IF this.descuento<>0
             .addprop("Descuento", this._fixstr(TRANSFORM(this.descuento, "@Z 999999999999.99")))
             .addprop("motivoDescuento", this._fixstr(this.motivodescuento), .T.)
          ENDIF
          IF this.tipodecomprobante='I' .OR. this.tipodecomprobante='N'
             .addprop("Total", this._fixstr(STR(this.total, 15, 2)))
          ELSE
             .addprop("Total", this._fixstr('0'))
          ENDIF
          .addprop("TipoDeComprobante", this._fixstr(this.tipodecomprobante))
          .addprop("NoCertificado", this.nocertificado)
          .addprop("Certificado", this.certificado)
          .addprop("Sello", this.sello)
       ENDWITH
       .xml._comprobante.addnode("Emisor")
       WITH .xml._comprobante._emisor
          .addprop("Rfc", CHRTRAN(this._fixstr(this.emisor.rfc), ".- ", ""))
          .addprop("RegimenFiscal", this._fixstr(this.emisor.regimenfiscal))
          IF INLIST(nversion, cfdversions.cfd_20, cfdversions.cfdi_30)
             .addprop("Nombre", this._fixstr(this.emisor.nombre))
          ELSE
             IF  .NOT. EMPTY(this.emisor.nombre)
                .addprop("Nombre", this._fixstr(this.emisor.nombre))
             ENDIF
          ENDIF
          IF  .NOT. EMPTY(this.emisor.domiciliofiscal.pais)
             .addnode("DomicilioFiscal")
             WITH ._domiciliofiscal
                .addprop("calle", this._fixstr(this.emisor.domiciliofiscal.calle))
                .addprop("noExterior", this._fixstr(this.emisor.domiciliofiscal.noexterior), .T.)
                .addprop("noInterior", this._fixstr(this.emisor.domiciliofiscal.nointerior), .T.)
                .addprop("colonia", this._fixstr(this.emisor.domiciliofiscal.colonia), .T.)
                .addprop("localidad", this._fixstr(this.emisor.domiciliofiscal.localidad), .T.)
                .addprop("referencia", this._fixstr(this.emisor.domiciliofiscal.referencia), .T.)
                .addprop("municipio", this._fixstr(this.emisor.domiciliofiscal.municipio))
                .addprop("estado", this._fixstr(this.emisor.domiciliofiscal.estado))
                .addprop("pais", this._fixstr(this.emisor.domiciliofiscal.pais))
                .addprop("CodigoPostal", this._fixstr(this.emisor.domiciliofiscal.codigopostal))
             ENDWITH
          ENDIF
          IF  .NOT. EMPTY(this.emisor.expedidoen.pais)
             .addnode("ExpedidoEn")
             WITH ._expedidoen
                .addprop("CodigoPostal", this._fixstr(this.emisor.expedidoen.codigopostal))
             ENDWITH
          ENDIF
       ENDWITH
       .xml._comprobante.addnode("Receptor")
       WITH .xml._comprobante._receptor
          .addprop("Rfc", CHRTRAN(this._fixstr(this.receptor.rfc), ".- ", ""))
          .addprop("UsoCFDI", this._fixstr(this.receptor.usocfdi))
          IF  .NOT. EMPTY(this.receptor.nombre)
             .addprop("Nombre", this._fixstr(this.receptor.nombre))
          ENDIF
          IF  .NOT. EMPTY(this.receptor.domiciliofiscal.pais)
             .addnode("Domicilio")
             WITH ._domicilio
                .addprop("calle", this._fixstr(this.receptor.domiciliofiscal.calle), .T.)
                .addprop("noExterior", this._fixstr(this.receptor.domiciliofiscal.noexterior), .T.)
                .addprop("noInterior", this._fixstr(this.receptor.domiciliofiscal.nointerior), .T.)
                .addprop("colonia", this._fixstr(this.receptor.domiciliofiscal.colonia), .T.)
                .addprop("localidad", this._fixstr(this.receptor.domiciliofiscal.localidad), .T.)
                .addprop("referencia", this._fixstr(this.receptor.domiciliofiscal.referencia), .T.)
                .addprop("municipio", this._fixstr(this.receptor.domiciliofiscal.municipio), .T.)
                .addprop("estado", this._fixstr(this.receptor.domiciliofiscal.estado), .T.)
                .addprop("pais", this._fixstr(this.receptor.domiciliofiscal.pais))
                .addprop("codigoPostal", this._fixstr(this.receptor.domiciliofiscal.codigopostal), .T.)
             ENDWITH
          ENDIF
       ENDWITH
       LOCAL i, oitem, onodo, oxnodo
       .xml._comprobante.addnode("Conceptos")
       WITH .xml._comprobante._conceptos
          .createnodelinks = .F.
          FOR i = 1 TO this.conceptos.count
             oitem = this.conceptos.items(i)
             onodo = .addnode("Concepto")
             WITH onodo
                .addprop("ClaveProdServ", this._fixstr(oitem.claveprodserv))
                .addprop("NoIdentificacion", this._fixstr(oitem.noidentificacion), .T.)
                IF this.tipodecomprobante='P'
                   .addprop("Cantidad", this._fixstr(oitem.cantidad))
                ELSE
                   .addprop("Cantidad", this._fixstr(STR(oitem.cantidad, 10, 2)))
                ENDIF
                .addprop("ClaveUnidad", this._fixstr(oitem.claveunidad))
                .addprop("Unidad", this._fixstr(oitem.unidad), .T.)
                .addprop("Descripcion", this._fixstr(oitem.descripcion))
                IF this.tipodecomprobante='P'
                   .addprop("ValorUnitario", this._fixstr(oitem.valorunitario))
                   .addprop("Importe", this._fixstr(oitem.importe))
                ELSE
                   .addprop("ValorUnitario", this._fixstr(STR(oitem.valorunitario, 15, 2)))
                   .addprop("Importe", this._fixstr(STR(oitem.importe, 15, 2)))
                ENDIF
                IF this.tipodecomprobante='N'
                   .addprop("Descuento", this._fixstr(oitem.descuento))
                ELSE
                   IF oitem.descuento>0
                      .addprop("Descuento", this._fixstr(STR(oitem.descuento, 10, 2)), .T.)
                   ENDIF
                ENDIF
                IF this.tipodecomprobante='I'
                   onodo = .addnode("Impuestos")
                   WITH onodo
                      onodo = .addnode("Traslados")
                      WITH onodo
                         .createnodelinks = .F.
                         onodo = .addnode("Traslado")
                         onodo.addprop("Base", this._fixstr(STR(oitem.base, 15, 2)))
                         onodo.addprop("Impuesto", this._fixstr(oitem.impuesto))
                         onodo.addprop("TipoFactor", this._fixstr(oitem.tipofactor))
                         onodo.addprop("TasaOCuota", this._fixstr(STR((oitem.tasaocuota/100), 10, 6)))
                         onodo.addprop("Importe", this._fixstr(STR(oitem.iimporte, 15, 2)))
                      ENDWITH
                   ENDWITH
                ENDIF
                IF  .NOT. EMPTY(oitem.cuentapredial.numero)
                   .addnode("CuentaPredial")
                   WITH .nodes(1)
                      .addprop("numero", this._fixstr(oitem.cuentapredial.numero))
                   ENDWITH
                ENDIF
                IF  .NOT. ISNULL(oitem.complemento) .AND. LOWER(oitem.complemento.parentclass)=="icfdaddenda"
                   onodo = .addnode("ComplementoConcepto")
                   onodo.data = oitem.complemento.tostring()
                   IF  .NOT. EMPTY(oitem.complemento.nstag)
                      IF  .NOT. oparser.xml._comprobante.isprop("xmlns:"+oitem.complemento.nstag)
                         oparser.xml._comprobante.addprop("xmlns:"+oitem.complemento.nstag, oitem.complemento.nsurl)
                      ENDIF
                   ENDIF
                   IF  .NOT. EMPTY(oitem.complemento.schemalocation)
                      WITH oparser.xml._comprobante.props("xsi:schemaLocation")
                         .value = .value+SPACE(1)+oitem.complemento.schemalocation
                      ENDWITH
                   ENDIF
                ENDIF
             ENDWITH
          ENDFOR
       ENDWITH
       IF this.tipodecomprobante='I'
          .xml._comprobante.addnode("Impuestos")
          WITH .xml._comprobante._impuestos
             IF this.impuestos.retenciones.count>0
                .addprop("TotalImpuestosRetenidos", this._fixstr(STR(this.impuestos.totalimpuestosretenidos, 15, 2)))
                .addnode("Retenciones")
                WITH ._retenciones
                   .createnodelinks = .F.
                   FOR i = 1 TO this.impuestos.retenciones.count
                      oitem = this.impuestos.retenciones.items(i)
                      onodo = .addnode("Retencion")
                      onodo.addprop("Impuesto", this._fixstr(oitem.impuesto))
                      onodo.addprop("Importe", this._fixstr(STR(oitem.importe, 15, 2)))
                   ENDFOR
                ENDWITH
             ENDIF
             IF this.impuestos.traslados.count>0
                .addprop("TotalImpuestosTrasladados", this._fixstr(STR(this.impuestos.totalimpuestostrasladados, 15, 2)))
                .addnode("Traslados")
                WITH ._traslados
                   .createnodelinks = .F.
                   FOR i = 1 TO this.impuestos.traslados.count
                      oitem = this.impuestos.traslados.items(i)
                      onodo = .addnode("Traslado")
                      onodo.addprop("Impuesto", this._fixstr(oitem.impuesto))
                      onodo.addprop("TipoFactor", this._fixstr(oitem.tipofactor))
                      onodo.addprop("TasaOCuota", this._fixstr(STR((oitem.tasaocuota/100), 10, 6)))
                      onodo.addprop("Importe", this._fixstr(STR(oitem.importe, 15, 2)))
                   ENDFOR
                ENDWITH
             ENDIF
          ENDWITH
       ENDIF
       IF  .NOT. ISNULL(this.addenda) .AND. LOWER(this.addenda.parentclass)=="icfdaddenda"
          onodo = oparser.xml._comprobante.addnode(this.addenda.nodename)
          onodo.data = this.addenda.tostring()
          IF  .NOT. EMPTY(this.addenda.nstag)
             oparser.xml._comprobante.addprop("xmlns:"+this.addenda.nstag, this.addenda.nsurl)
          ENDIF
          IF  .NOT. EMPTY(this.addenda.schemalocation)
             WITH oparser.xml._comprobante.props("xsi:schemaLocation")
                .value = .value+SPACE(1)+this.addenda.schemalocation
             ENDWITH
          ENDIF
       ENDIF
       IF this.tipodecomprobante='P'
          .xml._comprobante.addnode("Complemento")
          WITH .xml._comprobante._complemento
             onodo = oparser.xml._comprobante._complemento.addnode("pago10:Pagos")
             WITH oparser.xml._comprobante._complemento._pagos
                .addprop('Version', this._fixstr('1.0'))
                onodo = oparser.xml._comprobante._complemento._pagos.addnode("pago10:Pago")
                WITH oparser.xml._comprobante._complemento._pagos._pago
                   onodo.addprop("FechaPago", this._fixstr(xfecpago))
                   onodo.addprop("FormaPagoP", this._fixstr(xfdpago))
                   onodo.addprop("MonedaP", this._fixstr(xmoneda))
                   onodo.addprop("Monto", this._fixstr(STR(m.imp_net, 15, 2)))
                   onodo = oparser.xml._comprobante._complemento._pagos._pago.addnode("pago10:DoctoRelacionado")
                   WITH oparser.xml._comprobante._complemento._pagos._pago._doctorelacionado
                      onodo.addprop("IdDocumento", this._fixstr(xtimbre))
                      onodo.addprop("MonedaDR", this._fixstr(xmoneda))
                      onodo.addprop("MetodoDePagoDR", this._fixstr(xmetpago))
                      onodo.addprop("NumParcialidad", this._fixstr(xnumparc))
                      onodo.addprop("ImpSaldoAnt", this._fixstr(STR(xsdoant, 15, 2)))
                      onodo.addprop("ImpPagado", this._fixstr(STR(m.imp_net, 15, 2)))
                      onodo.addprop("ImpSaldoInsoluto", this._fixstr(STR(xinsoluto, 15, 2)))
                   ENDWITH
                ENDWITH
             ENDWITH
          ENDWITH
       ENDIF
       swfetim = .F.
       IF swfetim
          IF this.tipodecomprobante<>'P'
             .xml._comprobante.addnode("Complemento")
          ENDIF
          WITH .xml._comprobante._complemento
             .addnode("tfd:TimbreFiscalDigital")
          ENDWITH
          WITH .xml._comprobante._complemento._timbrefiscaldigital
             .addprop('Version', this._fixstr('1.1'))
             .addprop('xsi:schemaLocation', this._fixstr(xtlcencabtfd))
             .addprop('xmlns:tfd', this._fixstr(xtlcpietfd))
             .addprop("UUID", this._fixstr(xtlcfolio))
             .addprop("RfcProvCertif", this._fixstr(xtlrfcprocer))
             .addprop("FechaTimbrado", this._fixstr(xtlcfechatimbrado))
             .addprop("SelloCFD", this._fixstr(xtlcsellocfd))
             .addprop("NoCertificadoSAT", this._fixstr(xtlccertificadosat))
             .addprop("SelloSAT", this._fixstr(xtlcsellosat))
          ENDWITH
       ENDIF
       .save(pcarchivo)
       LOCAL cbuff
       cbuff = FILETOSTR(pcarchivo)
       cbuff = STRTRAN(cbuff, ">"+CHR(13)+CHR(10), ">")
       cbuff = STRTRAN(cbuff, "?>", "?>"+CHR(13)+CHR(10))
       cbuff = cfdasc2utf8(cbuff)
       IF cfdconf.incluirbom
          LOCAL cbom
          cbom = CHR(239)+CHR(187)+CHR(191)
          STRTOFILE(cbom+cbuff, pcarchivo)
       ELSE
          STRTOFILE(cbuff, pcarchivo)
       ENDIF
       cbuff = ""
       IF plvalidar
          IF  .NOT. cfdvalidarxml(pcarchivo, pcarchivokey, pcpassword, pcmetodo, this.ubicacionopenssl)
             RETURN .F.
          ENDIF
       ENDIF
       RETURN .T.
    ENDWITH
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDPersona AS Custom
 rfc = ""
 regimenfiscal = ""
 usocfdi = ""
 expedidoen = .NULL.
 nombre = ""
 domiciliofiscal = .NULL.
**
   PROCEDURE Init
    this.domiciliofiscal = CREATEOBJECT("CFDDireccion")
    this.expedidoen = CREATEOBJECT("CFDDireccion")
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDDireccion AS Custom
 calle = ""
 municipio = ""
 estado = ""
 pais = ""
 codigopostal = ""
 noexterior = ""
 nointerior = ""
 colonia = ""
 localidad = ""
 referencia = ""
ENDDEFINE
**
DEFINE CLASS CfdiRelacionados AS CFDCollection
**
   FUNCTION Add
    LPARAMETERS pcuuid
    LOCAL ocfdirelacionado
    ocfdirelacionado = CREATEOBJECT("CfdiRelacionado")
    WITH ocfdirelacionado
       .uuid = pcuuid
    ENDWITH
    RETURN DODEFAULT(ocfdirelacionado)
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CfdiRelacionado AS Custom
 uuid = ""
ENDDEFINE
**
DEFINE CLASS CFDConceptos AS CFDCollection
**
   FUNCTION Add
    LPARAMETERS pcclaveprodserv, pcnoidentificacion, pncantidad, pcclaveunidad, pcunidad, pcdescripcion, pnpu, pnimporte, pndescuento, pnbase, pcimpuesto, pctipofactor, pntasaocuota, pniimporte
    LOCAL oconcepto
    oconcepto = CREATEOBJECT("CFDConcepto")
    WITH oconcepto
       .claveprodserv = pcclaveprodserv
       .noidentificacion = pcnoidentificacion
       .cantidad = pncantidad
       .claveunidad = pcclaveunidad
       .unidad = pcunidad
       .descripcion = pcdescripcion
       .valorunitario = pnpu
       .importe = pnimporte
       .base = pnbase
       .impuesto = pcimpuesto
       .tipofactor = pctipofactor
       .tasaocuota = pntasaocuota
       .iimporte = pniimporte
       IF pndescuento>0
          ? "SI"
          WAIT
          .descuento = pndescuento
       ENDIF
    ENDWITH
    RETURN DODEFAULT(oconcepto)
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDConcepto AS Custom
 claveprodserv = ''
 noidentificacion = ''
 cantidad = 0.00 
 claveunidad = ''
 unidad = ""
 descripcion = ""
 valorunitario = 0.00 
 importe = 0.00 
 descuento = 0.00 
 base = 0.00 
 impuesto = ''
 tipofactor = ''
 tasaocuota = 0.00 
 iimporte = 0.00 
 impuestos = .NULL.
 informacionaduanera = .NULL.
 cuentapredial = .NULL.
 complemento = .NULL.
**
   PROCEDURE Init
    this.impuestos = CREATEOBJECT("CFDComplementoImpuestos")
    this.informacionaduanera = CREATEOBJECT("CFDInformacionAduanera")
    this.cuentapredial = CREATEOBJECT("CFDCuentaPredial")
    this.complemento = .NULL.
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDComplementoImpuestos AS Custom
 traslados = .NULL.
 retenciones = .NULL.
**
   PROCEDURE Init
    this.traslados = CREATEOBJECT('CFDComTraslados')
    this.retenciones = CREATEOBJECT('CFDComRetenciones')
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDComTraslados AS Custom
 traslado = .NULL.
**
   PROCEDURE Init
    this.traslado = CREATEOBJECT('CFDComTraslado')
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDComTraslado AS Custom
 base = 0.00 
 impuesto = ''
 tipofactor = ''
 tasaocuota = 0.00 
 importe = 0.00 
ENDDEFINE
**
DEFINE CLASS CFDComRetenciones AS Custom
 retencion = .NULL.
**
   PROCEDURE Init
    this.retencion = CREATEOBJECT('CFDComRetencion')
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDComRetencion AS Custom
 base = 0.00 
 impuesto = ''
 tipofactor = ''
 tasaocuota = 0.00 
 importe = 0.00 
ENDDEFINE
**
DEFINE CLASS CFDInformacionAduanera AS Custom
 numero = ""
 fecha = {}
 aduana = ""
ENDDEFINE
**
DEFINE CLASS CFDCuentaPredial AS Custom
 numero = ""
ENDDEFINE
**
DEFINE CLASS CFDImpuestos AS Custom
 totalimpuestosretenidos = 0.00 
 totalimpuestostrasladados = 0.00 
 retenciones = .NULL.
 traslados = .NULL.
**
   FUNCTION TotalImpuestosRetenidos_Access
    LOCAL i, ntotal
    ntotal = 0.00 
    FOR i = 1 TO this.retenciones.count
       ntotal = ntotal+this.retenciones.items(i).importe
    ENDFOR
    RETURN ntotal
   ENDFUNC
**
   PROCEDURE TotalImpuestosRetenidos_Assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   FUNCTION TotalImpuestosTrasladados_Access
    LOCAL i, ntotal
    ntotal = 0.00 
    FOR i = 1 TO this.traslados.count
       ntotal = ntotal+this.traslados.items(i).importe
    ENDFOR
    RETURN ntotal
   ENDFUNC
**
   PROCEDURE TotalImpuestosTrasladados_Assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   PROCEDURE Init
    this.retenciones = CREATEOBJECT("CFDRetenciones")
    this.traslados = CREATEOBJECT("CFDTraslados")
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDRetenciones AS CFDCollection
**
   FUNCTION Add
    LPARAMETERS pcimpuesto, pnimporte
    LOCAL oretencion
    oretencion = CREATEOBJECT("CFDRetencion")
    WITH oretencion
       .impuesto = pcimpuesto
       .importe = pnimporte
    ENDWITH
    RETURN DODEFAULT(oretencion)
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDTraslados AS CFDCollection
**
   FUNCTION Add
    LPARAMETERS pcimpuesto, pctipofactor, pntasaocuota, pnimporte
    LOCAL otraslado
    otraslado = CREATEOBJECT("CFDTraslado")
    WITH otraslado
       .impuesto = pcimpuesto
       .tipofactor = pctipofactor
       .tasaocuota = pntasaocuota
       .importe = pnimporte
    ENDWITH
    RETURN DODEFAULT(otraslado)
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDRetencion AS Custom
 impuesto = ''
 importe = 0.00 
ENDDEFINE
**
DEFINE CLASS CFDTraslado AS Custom
 impuesto = ''
 tipofactor = ''
 tasaocuota = 0.00 
 importe = 0.00 
ENDDEFINE
**
DEFINE CLASS CFDAddenda AS CFDCollection
**
   PROCEDURE Add
    LPARAMETERS pcnombre, pcvalor
    LOCAL oitem
    oitem = CREATEOBJECT("Custom")
    oitem.addproperty("Nombre", pcnombre)
    oitem.addproperty("Valor", pcvalor)
    DODEFAULT(oitem)
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS CFDCollection AS Custom
 HIDDEN ncount
 ncount = 0
 HIDDEN leoc
 leoc = .T.
 HIDDEN lboc
 lboc = .T.
 listindex = 0
 newitemclass = ""
 name = "cbasiccollection"
 count = .F.
 eoc = .F.
 boc = .F.
 current = .F.
 DIMENSION items[1, 1]
 PROTECTED aitems[1, 1]
**
   FUNCTION items_access
    LPARAMETERS m.nindex1, m.nindex2
    IF TYPE("m.nIndex1")="C"
       m.nindex1 = this.finditem(m.nindex1)
    ENDIF
    RETURN this.aitems(m.nindex1)
   ENDFUNC
**
   PROCEDURE items_assign
    LPARAMETERS vnewval, m.nindex1, m.nindex2
    IF TYPE("m.nIndex1")="C"
       m.nindex1 = this.finditem(m.nindex1)
    ENDIF
    IF BETWEEN(m.nindex1, 1, this.count)
       this.aitems[m.nindex1] = m.vnewval
    ENDIF
   ENDPROC
**
   FUNCTION count_access
    RETURN this.ncount
   ENDFUNC
**
   PROCEDURE count_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   FUNCTION add
    LPARAMETERS puvalue
    IF PARAMETERS()=0
       RETURN .F.
    ENDIF
    this.ncount = this.ncount+1
    DIMENSION this.aitems[this.ncount]
    this.aitems[this.ncount] = puvalue
    this.leoc = .F.
    this.lboc = .F.
    IF this.listindex=0
       this.listindex = 1
    ENDIF
    RETURN puvalue
   ENDFUNC
**
   FUNCTION remove
    LPARAMETERS puvalue
    IF PARAMETERS()=0
       RETURN .F.
    ENDIF
    LOCAL nindex
    nindex = this.finditem(puvalue)
    IF nindex>0
       RETURN this.removeitem(nindex)
    ELSE
       RETURN .F.
    ENDIF
   ENDFUNC
**
   PROCEDURE clear
    LOCAL i, uitem
    FOR i = 1 TO this.ncount
       uitem = this.aitems(i)
       IF TYPE("uItem")="O"
          RELEASE uitem
          this.aitems[i] = .NULL.
       ENDIF
    ENDFOR
    DIMENSION this.aitems[1]
    this.aitems[1] = .NULL.
    this.ncount = 0
    this.lboc = .T.
    this.leoc = .T.
    this.listindex = 0
   ENDPROC
**
   FUNCTION isitem
    LPARAMETERS puvalue, pcsearchprop
    IF PARAMETERS()=0
       RETURN .F.
    ENDIF
    RETURN (this.finditem(puvalue, pcsearchprop)<>0)
   ENDFUNC
**
   FUNCTION finditem
    LPARAMETERS puvalue, pcsearchprop
    IF PARAMETERS()=0 .OR. this.ncount=0
       RETURN 0
    ENDIF
    IF VARTYPE(pcsearchprop)<>"C"
       pcsearchprop = ""
    ENDIF
    LOCAL i, uitem, ctype1, npos
    npos = 0
    ctype1 = TYPE("puValue")
    FOR i = 1 TO this.ncount
       uitem = this.aitems(i)
       IF TYPE("uItem")="O"
          IF (ctype1="O" .AND. TYPE("uItem.Name")="C" .AND. TYPE("puVale.Name")="C" .AND. UPPER(uitem.name)==UPPER(puvalue.name)) .OR. (ctype1="C" .AND. TYPE("uItem.Name")="C" .AND. UPPER(uitem.name)==UPPER(puvalue)) .OR. (ctype1<>"O" .AND.  .NOT. EMPTY(pcsearchprop) .AND. TYPE("uItem."+pcsearchprop)=ctype1 .AND. EVALUATE("uItem."+pcsearchprop)==puvalue)
             npos = i
             EXIT
          ENDIF
       ELSE
          IF TYPE("uItem")=ctype1 .AND. ((ctype1<>"C" .AND. uitem=puvalue) .OR. (ctype1="C" .AND. uitem==puvalue))
             npos = i
             EXIT
          ENDIF
       ENDIF
    ENDFOR
    RETURN npos
   ENDFUNC
**
   FUNCTION removeitem
    LPARAMETERS nindex
    IF PARAMETERS()=0 .OR.  .NOT. BETWEEN(nindex, 1, this.ncount)
       RETURN .F.
    ENDIF
    LOCAL uitem
    uitem = this.aitems(nindex)
    IF TYPE("uItem")="O"
       RELEASE uitem
       this.aitems[nindex] = .NULL.
    ENDIF
    ADEL(this.aitems, nindex)
    this.ncount = this.ncount-1
    IF this.ncount>0
       DIMENSION this.aitems[this.ncount]
       IF this.ncount>this.listindex
          this.listindex = this.ncount
       ENDIF
    ELSE
       this.aitems[1] = .NULL.
       this.leoc = .T.
       this.lboc = .T.
       this.listindex = 0
    ENDIF
   ENDFUNC
**
   FUNCTION eoc_access
    RETURN this.leoc
   ENDFUNC
**
   PROCEDURE eoc_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   FUNCTION boc_access
    RETURN this.lboc
   ENDFUNC
**
   PROCEDURE boc_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   PROCEDURE first
    IF this.ncount=0
       RETURN
    ENDIF
    this.listindex = 1
    this.lboc = .F.
    this.leoc = .F.
   ENDPROC
**
   PROCEDURE next
    IF this.ncount=0
       RETURN
    ENDIF
    IF this.listindex<this.ncount
       this.listindex = this.listindex+1
       this.lboc = .F.
       this.leoc = .F.
    ELSE
       this.lboc = (this.ncount=1)
       this.leoc = .T.
    ENDIF
   ENDPROC
**
   PROCEDURE last
    IF this.ncount=0
       RETURN
    ENDIF
    this.listindex = this.ncount
    this.lboc = .F.
    this.leoc = .F.
   ENDPROC
**
   PROCEDURE previous
    IF this.ncount=0
       RETURN
    ENDIF
    IF this.listindex>1
       this.listindex = this.listindex-1
       this.lboc = .F.
       this.leoc = .F.
    ELSE
       this.lboc = .T.
       this.leoc = (this.ncount=1)
    ENDIF
   ENDPROC
**
   PROCEDURE listindex_assign
    LPARAMETERS vnewval
    IF TYPE("m.vNewVal")="N" .AND. BETWEEN(m.vnewval, 1, this.ncount)
       this.listindex = m.vnewval
       this.leoc = .F.
       this.lboc = .F.
    ENDIF
   ENDPROC
**
   FUNCTION current_access
    IF this.listindex=0
       RETURN .NULL.
    ELSE
       RETURN this.aitems(this.listindex)
    ENDIF
   ENDFUNC
**
   PROCEDURE current_assign
    LPARAMETERS vnewval
    IF this.listindex>0
       this.aitems[this.listindex] = m.vnewval
    ENDIF
   ENDPROC
**
   FUNCTION new
    IF EMPTY(this.newitemclass)
       RETURN .NULL.
    ENDIF
    LOCAL oitem
    oitem = kernel.cc.new(this.newitemclass)
    RETURN oitem
   ENDFUNC
**
   FUNCTION addifnew
    LPARAMETERS puvalue
    IF PARAMETERS()=0
       RETURN .F.
    ENDIF
    IF  .NOT. this.isitem(puvalue)
       this.add(puvalue)
    ENDIF
    RETURN puvalue
   ENDFUNC
**
ENDDEFINE
**
DEFINE CLASS CFDCertificado AS Custom
 archivo = ""
 valido = .F.
 vigente = .F.
 certificado = ""
 serial = ""
 vigentedesde = { :}
 vigentehasta = { :}
ENDDEFINE
**
DEFINE CLASS ICFDAddenda AS Custom
 nodename = "Addenda"
 schemalocation = ""
 nstag = ""
 nsurl = ""
**
   PROCEDURE ToString
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS XmlParser AS custom
 indentstring = ((CHR(9)))
 name = "xmlparser"
 filename = .F.
 xml = .F.
 createnodelinks = .T.
**
   FUNCTION new
    this.xml = CREATEOBJECT("XMLNodeClass")
    this.xml.name = "ROOT"
    this.xml.createnodelinks = this.createnodelinks
    this.xml.indentstring = this.indentstring
    this.filename = ""
    RETURN this.xml
   ENDFUNC
**
   FUNCTION open
    LPARAMETERS pcfilename
    LOCAL npos, cdata, nsize
    LOCAL xmldata[1]
    cdata = FILETOSTR(pcfilename)
    IF ATC("<?XML ", cdata)<>0
       npos = ATC("?>", cdata)
       cdata = SUBSTR(cdata, npos+2)
    ENDIF
    nsize = ALINES(xmldata, cdata)
    this.filename = pcfilename
    LOCAL i, j, k, clin, ctext, onode, oparent, nopentag, nclosetag1, nclosetag2, nclosetag3
    LOCAL cprops, cdata, lpropmode, ldatamode, lclosedtag, lendedtag, cname
    oparent = CREATEOBJECT("XMLNodeClass")
    oparent.name = "ROOT"
    FOR i = 1 TO nsize
       clin = xmldata(i)
       nopentag = AT("<", clin)
       nclosetag1 = AT("</", clin)
       nclosetag2 = AT(">", clin)
       nclosetag3 = AT("/>", clin)
       DO CASE
          CASE nopentag<>0 .AND. (nclosetag1=0 .OR. nclosetag1>nopentag)
             onode = CREATEOBJECT("XMLNodeClass")
             onode.cprops = ""
             onode.cdata = ""
             onode.lpropmode = .F.
             onode.ldatamode = .F.
             onode.createnodelinks = this.createnodelinks
             ctext = SUBSTR(clin, nopentag+1)
             j = AT("/>", ctext)
             j = IIF(j=0, AT(">", ctext), j)
             lclosedtag = (j<>0)
             IF lclosedtag
                ctext = LEFT(ctext, j-1)
             ENDIF
             cname = ""
             j = AT(" ", ctext)
             IF j=0
                cname = ALLTRIM(ctext)
             ELSE
                cname = ALLTRIM(LEFT(ctext, j-1))
                onode.cprops = SUBSTR(ctext, j+1)
                onode.lpropmode = ( .NOT. lclosedtag)
             ENDIF
             IF AT(":", cname)=0
                onode.name = cname
                onode.namespace = ""
             ELSE
                onode.name = SUBSTR(cname, AT(":", cname)+1)
                onode.namespace = LEFT(cname, AT(":", cname)-1)
             ENDIF
             IF  .NOT. ISNULL(oparent)
                oparent.addnode(onode)
             ENDIF
             IF ( .NOT. EMPTY(onode.cprops)) .AND. ( .NOT. onode.lpropmode)
                this.isplitprops(onode.cprops, onode)
             ENDIF
             IF lclosedtag .AND. nclosetag3=0
                ctext = SUBSTR(clin, nclosetag2+1)
                IF nclosetag1<>0 .AND. OCCURS("</", ctext)=1 .AND. ATC("</"+onode.name+">", ctext)<>0
                   ctext = LEFT(ctext, AT("</", ctext)-1)
                   onode.data = ctext
                   onode.ldatamode = .F.
                ELSE
                   onode.cdata = ctext
                   onode.ldatamode = .T.
                ENDIF
             ENDIF
             IF onode.lpropmode .OR. onode.ldatamode
                oparent = onode
             ELSE
                IF  .NOT. ISNULL(onode.parentnode)
                   oparent = onode.parentnode
                ENDIF
             ENDIF
          CASE ISNULL(oparent)
          CASE oparent.lpropmode .AND. nclosetag1=0 .AND. nclosetag2=0
             oparent.cprops = oparent.cprops+" "+clin
          CASE oparent.lpropmode .AND. nclosetag1=0 .AND. nclosetag2<>0
             oparent.cprops = oparent.cprops+" "+LEFT(clin, nclosetag2-1)
             oparent.lpropmode = .F.
             this.isplitprops(oparent.cprops, oparent)
             ctext = SUBSTR(clin, nclosetag2+1)
             onode.cdata = ctext
             onode.ldatamode = .T.
          CASE oparent.lpropmode .AND. nclosetag1<>0 .AND. nclosetag2<>0
             oparent.cprops = oparent.cprops+" "+LEFT(clin, nclosetag2-1)
             oparent.lpropmode = .F.
             this.isplitprops(oparent.cprops, oparent)
             ctext = SUBSTR(clin, nclosetag2+1)
             ctext = SUBSTR(ctext, AT("</", ctext)-1)
             onode.cdata = ctext
             IF  .NOT. ISNULL(oparent.parentnode)
                oparent = oparent.parentnode
             ENDIF
          CASE oparent.ldatamode .AND. nclosetag1=0
             oparent.cdata = oparent.cdata+IIF(EMPTY(oparent.cdata), "", CHR(13)+CHR(10))+clin
          CASE oparent.ldatamode .AND. nclosetag1<>0
             oparent.cdata = oparent.cdata+IIF(EMPTY(oparent.cdata), "", CHR(13)+CHR(10))+LEFT(clin, nclosetag1-1)
             oparent.data = oparent.cdata
             oparent.ldatamode = .F.
             IF  .NOT. ISNULL(oparent.parentnode)
                oparent = oparent.parentnode
             ENDIF
          OTHERWISE
       ENDCASE
    ENDFOR
    this.xml = oparent
    RETURN this.xml
   ENDFUNC
**
   FUNCTION save
    LPARAMETERS pcfilename
    IF VARTYPE(pcfilename)="C"
       this.filename = pcfilename
    ENDIF
    IF EMPTY(this.filename)
       RETURN .F.
    ENDIF
    SET TEXTMERGE TO (this.filename) NOSHOW
    SET TEXTMERGE ON
    \\<?xml version="1.0" encoding="UTF-8"?>
    LOCAL i
    FOR i = 1 TO this.xml.nodecount
       this.isavenode(this.xml.nodes(i))
    ENDFOR
    SET TEXTMERGE OFF
    SET TEXTMERGE TO
   ENDFUNC
**
   FUNCTION ToString
    LPARAMETERS pnmargin
    LOCAL cxml, ccrlf, i
    ccrlf = CHR(13)+CHR(10)
    cxml = ""
    FOR i = 1 TO this.xml.nodecount
       IF i>1
          cxml = cxml+ccrlf
       ENDIF
       cxml = cxml+this.xml.nodes(i).tostring(pnmargin)
    ENDFOR
    RETURN cxml
   ENDFUNC
**
   HIDDEN PROCEDURE isavenode
    LPARAMETERS ponode, pnmargin
    LOCAL ndeep, calign, i, uvalue
    ndeep = OCCURS("\", ponode.fullpath)
    calign = REPLICATE(this.indentstring, ndeep)
    IF cfdevl(pnmargin, 0)>0
       calign = REPLICATE(this.indentstring, pnmargin)+calign
    ENDIF
    \<<cAlign>><<"<"+poNode.nodeName>>
    FOR i = 1 TO ponode.propcount
       IF i=1
          \\<<space(1)>>
       ENDIF
       uvalue = ponode.props(i).value
       DO CASE
          CASE VARTYPE(uvalue)="C"
             uvalue = '"'+uvalue+'"'
          CASE VARTYPE(uvalue)="D"
             uvalue = '{'+DTOC(uvalue)+'}'
          CASE VARTYPE(uvalue)="T"
             uvalue = '{'+TTOC(uvalue)+'}'
          CASE VARTYPE(uvalue)="N" .AND. INT(uvalue)<>uvalue
             uvalue = ALLTRIM(STR(uvalue, 30, 6))
          CASE VARTYPE(uvalue)="N" .AND. INT(uvalue)=uvalue
             uvalue = ALLTRIM(STR(uvalue, 30))
          CASE VARTYPE(uvalue)="L"
             uvalue = IIF(uvalue, ".T.", ".F.")
          OTHERWISE
             uvalue = TRANSFORM(uvalue, "")
       ENDCASE
       \\<<poNode.Props(i).Id>>=<<uValue>>
       IF i<ponode.propcount
          \\<<space(1)>>
       ENDIF
    ENDFOR
    IF ponode.propcount=0 .AND. ponode.nodecount=0
       IF ISNULL(ponode.data)
          \\<<"/>">>
       ELSE
          \\<<">">><<poNode.Data>><<+"</"+poNode.nodeName+">">>
       ENDIF
    ELSE
       IF ponode.nodecount=0
          IF ISNULL(ponode.data)
             \\/>
          ELSE
             \\><<poNode.Data>><<"</"+poNode.Id+">">>
          ENDIF
       ELSE
          IF ISNULL(ponode.data)
             \\>
          ELSE
             \\><<poNode.Data>>
          ENDIF
          FOR i = 1 TO ponode.nodecount
             this.isavenode(ponode.nodes(i), pnmargin)
          ENDFOR
          \<<cAlign>><<"</"+poNode.nodeNAme+">">>
       ENDIF
    ENDIF
   ENDPROC
**
   HIDDEN PROCEDURE isplitprops
    LPARAMETERS pcparamline, ponode
    LOCAL i, ncount, nblockdeepth, cchar, lquoted, ldoublequoted
    ncount = LEN(pcparamline)
    nblockdeepth = 0
    lquoted = .F.
    ldoublequoted = .F.
    FOR i = 1 TO ncount
       cchar = SUBSTR(pcparamline, i, 1)
       DO CASE
          CASE INLIST(cchar, "(", "{", "[")
             nblockdeepth = nblockdeepth+1
          CASE INLIST(cchar, ")", "}", "]") .AND. nblockdeepth>0
             nblockdeepth = nblockdeepth-1
          CASE cchar$'"'
             ldoublequoted = ( .NOT. ldoublequoted)
          CASE cchar$[']
             lquoted = ( .NOT. lquoted)
          CASE cchar="=" .AND. nblockdeepth<=0 .AND. ( .NOT. lquoted) .AND.  .NOT. (ldoublequoted)
             pcparamline = STUFF(pcparamline, i, 1, "|")
          CASE cchar="=" .AND. nblockdeepth>0 .OR. lquoted .OR. ldoublequoted
       ENDCASE
    ENDFOR
    LOCAL cprop, cvalue, npos, cdata, j, oprop
    cdata = pcparamline
    ncount = OCCURS("|", cdata)
    FOR i = 1 TO ncount
       npos = AT("|", cdata)
       cprop = ALLTRIM(LEFT(cdata, npos-1))
       cvalue = SUBSTR(cdata, npos+1)
       IF i<ncount
          npos = AT("|", cvalue)
          j = npos-1
          DO WHILE j>1 .AND.  .NOT. EMPTY(SUBSTR(cvalue, j, 1))
             j = j-1
          ENDDO
          IF j>1
             cdata = SUBSTR(cvalue, j+1)
             cvalue = ALLTRIM(LEFT(cvalue, j))
          ENDIF
       ENDIF
       ponode.addprop(cprop, EVALUATE(cvalue))
    ENDFOR
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS XmlNodeClass AS custom
 data = .NULL.
 parentnode = .NULL.
 propcount = 0
 nodecount = 0
 HIDDEN npropcount
 npropcount = 0
 HIDDEN nnodecount
 nnodecount = 0
 cprops = ""
 cdata = ""
 fullpath = ""
 name = "xmlnodeclass"
 lpropmode = .F.
 ldatamode = .F.
 namespace = ""
 nodename = ""
 createnodelinks = .T.
 indentstring = ""
 DIMENSION props[1, 1]
 DIMENSION nodes[1, 1]
**
   HIDDEN FUNCTION propcount_access
    RETURN this.npropcount
   ENDFUNC
**
   HIDDEN PROCEDURE propcount_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   HIDDEN FUNCTION nodecount_access
    RETURN this.nnodecount
   ENDFUNC
**
   HIDDEN PROCEDURE nodecount_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   HIDDEN FUNCTION props_access
    LPARAMETERS puindex
    IF this.propcount=0
       RETURN .NULL.
    ENDIF
    IF VARTYPE(puindex)="N"
       RETURN this.props(puindex)
    ELSE
       RETURN this.props(this.findprop(puindex))
    ENDIF
   ENDFUNC
**
   HIDDEN FUNCTION nodes_access
    LPARAMETERS puindex
    IF this.nodecount=0
       RETURN .NULL.
    ENDIF
    LOCAL nindex, i
    IF TYPE("puIndex")="N"
       nindex = puindex
    ELSE
       IF ATC("\", puindex)=0
          nindex = this.findnode(puindex)
       ELSE
          nindex = this.getnode(puindex)
       ENDIF
    ENDIF
    RETURN this.nodes(nindex)
   ENDFUNC
**
   HIDDEN FUNCTION nodeName_access
    RETURN IIF(EMPTY(this.namespace), "", this.namespace+":")+this.name
   ENDFUNC
**
   HIDDEN PROCEDURE nodeName_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   FUNCTION findprop
    LPARAMETERS pcpropname
    LOCAL i, nindex
    nindex = 0
    FOR i = 1 TO this.propcount
       IF UPPER(ALLTRIM(this.props(i).id))==UPPER(ALLTRIM(pcpropname))
          nindex = i
          EXIT
       ENDIF
    ENDFOR
    RETURN nindex
   ENDFUNC
**
   FUNCTION isprop
    LPARAMETERS pcpropname
    RETURN (this.findprop(pcpropname)<>0)
   ENDFUNC
**
   FUNCTION addprop
    LPARAMETERS pcname, puvalue, ploptional
    IF EMPTY(puvalue) .AND. ploptional
       RETURN .NULL.
    ENDIF
    LOCAL oprop, ncount, i
    LOCAL aprops[1]
    ncount = ALINES(aprops, STRTRAN(pcname, ",", CHR(13)+CHR(10)))
    FOR i = 1 TO ncount
       oprop = CREATEOBJECT("XMLPropClass")
       oprop.id = aprops(i)
       oprop.value = puvalue
       this.npropcount = this.npropcount+1
       DIMENSION this.props[this.npropcount]
       this.props[this.npropcount] = oprop
    ENDFOR
    RETURN oprop
   ENDFUNC
**
   PROCEDURE removeprop
    LPARAMETERS puindex
    IF VARTYPE(puindex)="C"
       puindex = this.findprop(puindex)
    ENDIF
    IF BETWEEN(puindex, 1, this.npropcount)
       this.props[puindex] = .NULL.
       ADEL(this.props, puindex)
       this.npropcount = this.npropcount-1
    ENDIF
   ENDPROC
**
   FUNCTION getProp
    LPARAMETERS pcpropname, pudefaultvalue
    IF PCOUNT()=1
       pudefaultvalue = ""
    ENDIF
    LOCAL nindex
    nindex = this.findprop(pcpropname)
    IF nindex>0
       RETURN this.props(nindex).value
    ELSE
       RETURN pudefaultvalue
    ENDIF
   ENDFUNC
**
   PROCEDURE copypropsfrom
    LPARAMETERS podata
    LOCAL i, cprop
    FOR i = 1 TO this.propcount
       cprop = this.props(i).id
       IF TYPE("poData."+cprop)<>"U"
          this.props(cprop).value = EVALUATE("poData."+cprop)
       ENDIF
    ENDFOR
   ENDPROC
**
   FUNCTION getnode
    LPARAMETERS pcnodefullpath
    LOCAL anodes[1]
    LOCAL i, ncount, nindex
    ncount = ALINES(anodes, STRTRAN(pcnodefullpath, "\", CHR(13)+CHR(10)))
    onode = this
    FOR i = 1 TO ncount
       IF UPPER(anodes(i))==UPPER(this.name)
          LOOP
       ENDIF
       nindex = onode.findnode(anodes(i))
       IF nindex=0
          EXIT
       ENDIF
       onode = onode.nodes(nindex)
    ENDFOR
    RETURN onode
   ENDFUNC
**
   FUNCTION findnode
    LPARAMETERS pcnodename
    LOCAL i, nindex
    nindex = 0
    FOR i = 1 TO this.nodecount
       IF UPPER(ALLTRIM(this.nodes(i).name))==UPPER(ALLTRIM(pcnodename))
          nindex = i
          EXIT
       ENDIF
    ENDFOR
    RETURN nindex
   ENDFUNC
**
   FUNCTION isnode
    LPARAMETERS pcpropname
    RETURN (this.findnode(pcpropname)<>0)
   ENDFUNC
**
   FUNCTION addnode
    LPARAMETERS ponode, pcdata
    DO CASE
       CASE PCOUNT()=0
          ponode = CREATEOBJECT("XMLNodeClass")
       CASE VARTYPE(ponode)="C"
          LOCAL cname, cnamespace
          cname = ponode
          cnamespace = ""
          IF AT(":", cname)<>0
             cnamespace = LEFT(cname, AT(":", cname)-1)
             cname = SUBSTR(cname, AT(":", cname)+1)
          ENDIF
          ponode = CREATEOBJECT("XMLNodeClass")
          ponode.name = cname
          ponode.namespace = cnamespace
    ENDCASE
    ponode.createnodelinks = this.createnodelinks
    ponode.indentstring = this.indentstring
    IF EMPTY(ponode.namespace)
       ponode.namespace = this.namespace
    ENDIF
    this.nnodecount = this.nnodecount+1
    DIMENSION this.nodes[this.nnodecount]
    this.nodes[this.nnodecount] = ponode
    ponode.parentnode = this
    IF PCOUNT()=2
       ponode.data = pcdata
    ENDIF
    IF this.createnodelinks
       LOCAL cnodelink
       cnodelink = "_"+ponode.name
       IF TYPE("THIS."+cnodelink)="U"
          this.addproperty(cnodelink, ponode)
       ELSE
          STORE ponode TO ("THIS."+cnodelink)
       ENDIF
    ENDIF
    RETURN ponode
   ENDFUNC
**
   PROCEDURE removenode
    LPARAMETERS puindex
    IF VARTYPE(puindex)="C"
       puindex = this.findnode(puindex)
    ENDIF
    IF BETWEEN(puindex, 1, this.nnodecount)
       LOCAL cnodelink
       cnodelink = "_"+this.nodes(puindex).name
       STORE .NULL. TO ("THIS."+cnodelink)
       this.nodes[puindex] = .NULL.
       ADEL(this.nodes, puindex)
       this.nnodecount = this.nnodecount-1
    ENDIF
   ENDPROC
**
   HIDDEN FUNCTION fullpath_access
    LOCAL cpath, onode
    cpath = PROPER(this.name)
    onode = this
    DO WHILE ( .NOT. ISNULL(onode.parentnode))
       IF onode.parentnode.name<>"ROOT"
          cpath = PROPER(onode.parentnode.name)+"\"+cpath
       ENDIF
       onode = onode.parentnode
    ENDDO
    RETURN cpath
   ENDFUNC
**
   HIDDEN PROCEDURE fullpath_assign
    LPARAMETERS vnewval
**
** ReFox - este procedimiento es vacío **
**
   ENDPROC
**
   FUNCTION ToString
    LPARAMETERS pnmargin
    LOCAL ndeep, calign, i, uvalue, cresult
    ndeep = OCCURS("\", this.fullpath)
    ccrlf = CHR(13)+CHR(10)
    calign = REPLICATE(this.indentstring, ndeep)
    IF cfdevl(pnmargin, 0)>0
       calign = REPLICATE(this.indentstring, pnmargin)+calign
    ENDIF
    cresult = calign+"<"+this.nodename
    FOR i = 1 TO this.propcount
       IF i=1
          cresult = cresult+SPACE(1)
       ENDIF
       uvalue = this.props(i).value
       DO CASE
          CASE VARTYPE(uvalue)="C"
             uvalue = '"'+uvalue+'"'
          CASE VARTYPE(uvalue)="D"
             uvalue = '{'+DTOC(uvalue)+'}'
          CASE VARTYPE(uvalue)="T"
             uvalue = '{'+TTOC(uvalue)+'}'
          CASE VARTYPE(uvalue)="N" .AND. INT(uvalue)<>uvalue
             uvalue = ALLTRIM(STR(uvalue, 30, 6))
          CASE VARTYPE(uvalue)="N" .AND. INT(uvalue)=uvalue
             uvalue = ALLTRIM(STR(uvalue, 30))
          CASE VARTYPE(uvalue)="L"
             uvalue = IIF(uvalue, ".T.", ".F.")
          OTHERWISE
             uvalue = TRANSFORM(uvalue, "")
       ENDCASE
       cresult = cresult+this.props(i).id+"="+uvalue
       IF i<this.propcount
          cresult = cresult+SPACE(1)
       ENDIF
    ENDFOR
    IF this.propcount=0 .AND. this.nodecount=0
       IF ISNULL(this.data)
          cresult = cresult+"/>"
       ELSE
          cresult = cresult+">"+this.data+"</"+this.nodename+">"
       ENDIF
    ELSE
       IF this.nodecount=0
          IF ISNULL(this.data)
             cresult = cresult+"/>"
          ELSE
             cresult = cresult+">"+this.data+"</"+this.nodename+">"
          ENDIF
       ELSE
          IF ISNULL(this.data)
             cresult = cresult+">"
          ELSE
             cresult = cresult+">"+this.data
          ENDIF
          LOCAL cchildren
          FOR i = 1 TO this.nodecount
             cchildren = this.nodes(i).tostring(pnmargin)
             cresult = cresult+CHR(13)+CHR(10)+cchildren
          ENDFOR
          cresult = cresult+ccrlf+calign+"</"+this.nodename+">"
       ENDIF
    ENDIF
    RETURN cresult
   ENDFUNC
**
ENDDEFINE
**
FUNCTION Dec2Hex
 LPARAMETERS ndec
 LOCAL nresto, chex, ndig
 nresto = ndec
 chex = ""
 DO WHILE nresto>15
    ndig = MOD(nresto, 16)
    IF ndig<10
       chex = STR(ndig, 1)+chex
    ELSE
       chex = CHR(55+ndig)+chex
    ENDIF
    nresto = INT(nresto/16)
 ENDDO
 ndig = nresto
 IF ndig<10
    chex = STR(ndig, 1)+chex
 ELSE
    chex = CHR(55+ndig)+chex
 ENDIF
 RETURN chex
ENDFUNC
**
FUNCTION Hex2Dec
 LPARAMETERS chex
 LOCAL ndec, ndig, i, nexp
 ndec = 0
 FOR i = 1 TO LEN(chex)
    nexp = LEN(chex)-i
    ndig = SUBSTR(chex, i, 1)
    IF ndig$"ABCDEF"
       ndig = ASC(ndig)-55
    ELSE
       ndig = INT(VAL(ndig))
    ENDIF
    ndec = ndec+(ndig*(16**nexp))
 ENDFOR
 RETURN ndec
ENDFUNC
**
FUNCTION Str2Hex
 LPARAMETERS pcstr
 LOCAL chex, nlen, i
 chex = ""
 nlen = LEN(pcstr)
 FOR i = 1 TO nlen
    chex = chex+PADL(dec2hex(ASC(SUBSTR(pcstr, i, 1))), 2, "0")
 ENDFOR
 RETURN chex
ENDFUNC
**
FUNCTION Hex2Str
 LPARAMETERS pchexstr
 LOCAL nlen, cdec, i
 nlen = LEN(pchexstr)
 cdec = ""
 FOR i = 1 TO nlen STEP 2
    cdec = cdec+CHR(hex2dec(SUBSTR(pchexstr, i, 2)))
 ENDFOR
 RETURN cdec
ENDFUNC
**
FUNCTION CFDAsc2UTF8
 LPARAMETERS pcstring
 LOCAL cbuff, c, i, h, l
 cbuff = ""
 FOR i = 1 TO LEN(pcstring)
    c = ASC(SUBSTR(pcstring, i, 1))
    IF c<128
       cbuff = cbuff+CHR(c)
    ELSE
       h = BITOR(BITRSHIFT(c, 6), 192)
       l = BITOR(BITAND(c, 063), 128)
       cbuff = cbuff+CHR(h)+CHR(l)
    ENDIF
 ENDFOR
 RETURN cbuff
ENDFUNC
**
FUNCTION CFDUTF82Asc
 LPARAMETERS pcstring
 LOCAL cbuff, i, nasc, c
 nasc = 0
 cbuff = ""
 FOR i = 1 TO LEN(pcstring)
    c = ASC(SUBSTR(pcstring, i, 1))
    IF c<128
       IF nasc>0
          cbuff = cbuff+CHR(nasc)
          nasc = 0
       ENDIF
       cbuff = cbuff+CHR(c)
    ELSE
       IF BITTEST(c, 6)
          nsize = BITRSHIFT(BITAND(c, 096), 5)
          nasc = BITLSHIFT(BITCLEAR(BITCLEAR(c, 7), 6), 6*(nsize-1))
       ELSE
          nasc = BITOR(nasc, BITCLEAR(BITCLEAR(c, 7), 6))
       ENDIF
    ENDIF
 ENDFOR
 RETURN cbuff
ENDFUNC
**
FUNCTION CFDToCursor
 PARAMETER parchivoxml, pccursorprefix, pcopenssl
 cfdconf.ultimoerror = ""
 IF PCOUNT()=2
    pcopenssl = cfdconf.openssl
 ENDIF
 LOCAL archivoxml
 IF TYPE('pArchivoXML')<>'C'
    archivoxml = GETFILE("Comprobante digital(*.xml):XML", "Comprobante", "Abrir", 0, "Abrir archivo")
    IF EMPTY(archivoxml)
       cfdconf.ultimoerror = "Debe indicar el comprobante digital a leer"
       RETURN .F.
    ENDIF
 ELSE
    archivoxml = parchivoxml
 ENDIF
 PRIVATE cgenerales, cconceptos, caduanas, cretenciones, ctraslados, ccfdirelacion, ccretenciones, cctraslados
 IF EMPTY(pccursorprefix)
    pccursorprefix = "Q"
 ENDIF
 cgenerales = pccursorprefix+"DG"
 cconceptos = pccursorprefix+"CO"
 caduanas = pccursorprefix+"AD"
 cretenciones = pccursorprefix+"RE"
 ctraslados = pccursorprefix+"TR"
 cregimenes = pccursorprefix+"RF"
 ccfdirelacion = pccursorprefix+"RU"
 ccretenciones = pccursorprefix+"RC"
 cctraslados = pccursorprefix+"TC"
 LOCAL olnodes
 DO creafactura
 SELECT (cgenerales)
 INSERT INTO (cgenerales) (folio) VALUES ("   ")
 xdoc = CREATEOBJECT('MSXML2.DOMdocument')
 xdoc.load(archivoxml)
 IF (xdoc.parseerror.errorcode<>0)
    myerr = xdoc.parseerror
    cfdconf.ultimoerror = myerr.reason
    RETURN .F.
 ENDIF
 orootnode = xdoc.documentelement
 croottagname = orootnode.tagname
 IF croottagname="cfdi:Comprobante"
    lcprefijo = "cfdi:"
 ELSE
    lcprefijo = ""
 ENDIF
 LOCAL cveversionxml
 cveversionxml = ""
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Comprobante> no presente."
    RETURN .F.
 ENDIF
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="Version"
             UPDATE (cgenerales) SET versions = satributevalues
             cveversionxml = satributevalues
          CASE satributename="Serie"
             UPDATE (cgenerales) SET serie = satributevalues
          CASE satributename="Folio"
             UPDATE (cgenerales) SET folio = satributevalues
          CASE satributename="Fecha"
             UPDATE (cgenerales) SET fecha = satributevalues
          CASE satributename="Sello"
             UPDATE (cgenerales) SET sello = satributevalues
          CASE satributename="noAprobacion"
             UPDATE (cgenerales) SET noaprobaci = satributevalues
          CASE satributename="anoAprobacion"
             UPDATE (cgenerales) SET anoaprobac = satributevalues
          CASE satributename="FormaPago"
             UPDATE (cgenerales) SET formapago = satributevalues
          CASE satributename="NoCertificado"
             UPDATE (cgenerales) SET nocertific = satributevalues
          CASE satributename="Certificado"
             UPDATE (cgenerales) SET certific = satributevalues
          CASE satributename="CondicionesDePago"
             UPDATE (cgenerales) SET condicione = satributevalues
          CASE satributename="SubTotal"
             UPDATE (cgenerales) SET subtotal = VAL(satributevalues)
          CASE satributename="Descuento"
             UPDATE (cgenerales) SET descuento = VAL(satributevalues)
          CASE satributename="motivoDescuento"
             UPDATE (cgenerales) SET motivodesc = satributevalues
          CASE satributename="Total"
             UPDATE (cgenerales) SET total = VAL(satributevalues)
          CASE satributename="MetodoPago"
             UPDATE (cgenerales) SET metododepa = satributevalues
          CASE satributename="TipoDeComprobante"
             UPDATE (cgenerales) SET tipodecomp = satributevalues
          CASE satributename="TipoCambio"
             UPDATE (cgenerales) SET tipocambio = satributevalues
          CASE satributename="Moneda"
             UPDATE (cgenerales) SET moneda = satributevalues
          CASE satributename="MontoFolioFiscalOrig"
             UPDATE (cgenerales) SET mfoliofo = VAL(satributevalues)
          CASE satributename="FechaFolioFiscalOrig"
             UPDATE (cgenerales) SET ffoliofo = satributevalues
          CASE satributename="SerieFolioFiscalOrig"
             UPDATE (cgenerales) SET sfoliofo = satributevalues
          CASE satributename="FolioFiscalOrig"
             UPDATE (cgenerales) SET foliofo = satributevalues
          CASE satributename="NumCtaPago"
             UPDATE (cgenerales) SET numctapago = satributevalues
          CASE satributename="LugarExpedicion"
             UPDATE (cgenerales) SET lugarexp = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 SELECT (cgenerales)
 IF descuento>0 .AND. EMPTY(motivodesc)
    REPLACE motivodesc WITH "Descuento"
 ENDIF
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Emisor")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Emisor> no presente."
    RETURN .F.
 ENDIF
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="Rfc"
             satributevalues = CHRTRAN(satributevalues, ".- ", "")
             UPDATE (cgenerales) SET erfc = satributevalues
          CASE satributename="RegimenFiscal"
             UPDATE (cgenerales) SET eregimenfiscal = satributevalues
          CASE satributename="Nombre"
             UPDATE (cgenerales) SET enombre = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Emisor/"+lcprefijo+"DomicilioFiscal")
 IF olnodes.length>0
    FOR i = 0 TO olnodes.length-1
       FOR j = 0 TO olnodes.item(i).attributes.length-1
          satributename = olnodes.item(i).attributes.item(j).nodename
          satributevalues = olnodes.item(i).attributes.item(j).text
          DO CASE
             CASE satributename="calle"
                UPDATE (cgenerales) SET ecalle = satributevalues
             CASE satributename="noExterior"
                UPDATE (cgenerales) SET enoexterio = satributevalues
             CASE satributename="noInterior"
                UPDATE (cgenerales) SET enointerio = satributevalues
             CASE satributename="colonia"
                UPDATE (cgenerales) SET ecolonia = satributevalues
             CASE satributename="localidad"
                UPDATE (cgenerales) SET elocalidad = satributevalues
             CASE satributename="referencia"
                UPDATE (cgenerales) SET ereferen = satributevalues
             CASE satributename="municipio"
                UPDATE (cgenerales) SET emunicipio = satributevalues
             CASE satributename="estado"
                UPDATE (cgenerales) SET eestado = satributevalues
             CASE satributename="pais"
                UPDATE (cgenerales) SET epais = satributevalues
             CASE satributename="CodigoPostal"
                UPDATE (cgenerales) SET ecodigopos = satributevalues
          ENDCASE
       ENDFOR
    ENDFOR
 ELSE
    IF INLIST(cveversionxml, "2.0", "3.0")
       cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"DomicilioFiscal> del Emisor no presente."
       RETURN .F.
    ENDIF
 ENDIF
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Emisor/"+lcprefijo+"ExpedidoEn")
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="calle"
             UPDATE (cgenerales) SET xcalle = satributevalues
          CASE satributename="noExterior"
             UPDATE (cgenerales) SET xnoexterio = satributevalues
          CASE satributename="noInterior"
             UPDATE (cgenerales) SET xnointerio = satributevalues
          CASE satributename="colonia"
             UPDATE (cgenerales) SET xcolonia = satributevalues
          CASE satributename="localidad"
             UPDATE (cgenerales) SET xlocalidad = satributevalues
          CASE satributename="referencia"
             UPDATE (cgenerales) SET xreferen = satributevalues
          CASE satributename="municipio"
             UPDATE (cgenerales) SET xmunicipio = satributevalues
          CASE satributename="estado"
             UPDATE (cgenerales) SET xestado = satributevalues
          CASE satributename="pais"
             UPDATE (cgenerales) SET xpais = satributevalues
          CASE satributename="CodigoPostal"
             UPDATE (cgenerales) SET xcodigopos = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 SELECT (cregimenes)
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Emisor")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Emisor> no presente."
    RETURN .F.
 ENDIF
 leerdatos(xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Emisor"), '')
 SELECT (cgenerales)
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Receptor")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Receptor> no presente."
    RETURN .F.
 ENDIF
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="Rfc"
             satributevalues = CHRTRAN(satributevalues, ".- ", "")
             UPDATE (cgenerales) SET rrfc = satributevalues
          CASE satributename="Nombre"
             UPDATE (cgenerales) SET rnombre = satributevalues
          CASE satributename="UsoCFDI"
             UPDATE (cgenerales) SET usocfdi = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Receptor/"+lcprefijo+"Domicilio")
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="calle"
             UPDATE (cgenerales) SET rcalle = satributevalues
          CASE satributename="noExterior"
             UPDATE (cgenerales) SET rnoexterio = satributevalues
          CASE satributename="noInterior"
             UPDATE (cgenerales) SET rnointerio = satributevalues
          CASE satributename="colonia"
             UPDATE (cgenerales) SET rcolonia = satributevalues
          CASE satributename="localidad"
             UPDATE (cgenerales) SET rlocalidad = satributevalues
          CASE satributename="referencia"
             UPDATE (cgenerales) SET rreferen = satributevalues
          CASE satributename="municipio"
             UPDATE (cgenerales) SET rmunicipio = satributevalues
          CASE satributename="estado"
             UPDATE (cgenerales) SET restado = satributevalues
          CASE satributename="pais"
             UPDATE (cgenerales) SET rpais = satributevalues
          CASE satributename="CodigoPostal"
             UPDATE (cgenerales) SET rcodigopos = satributevalues
          CASE satributename="UsoCFDI"
             UPDATE (cgenerales) SET usocfdi = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 SELECT (cconceptos)
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Conceptos")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Conceptos> no presente."
    RETURN .F.
 ENDIF
 leerdatos(xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Conceptos"), '')
 SELECT (cgenerales)
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Impuestos/")
 IF olnodes.length<=0
    cfdconf.ultimoerror = "Comprobante inválido. Nodo <"+lcprefijo+"Impuestos> no presente."
    RETURN .F.
 ENDIF
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="TotalImpuestosTrasladados"
             UPDATE (cgenerales) SET totimptras = VAL(satributevalues)
          CASE satributename="TotalImpuestosRetenidos"
             UPDATE (cgenerales) SET totimpret = VAL(satributevalues)
       ENDCASE
    ENDFOR
 ENDFOR
 SELECT (cretenciones)
 leerdatos(xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Impuestos/"+lcprefijo+"Retenciones"), '')
 GOTO TOP
 SCAN
    DO CASE
       CASE ALLTRIM(EVALUATE(cretenciones+".Impuesto"))="ISR"
          UPDATE (cgenerales) SET retisr = "ISR", totretisr = EVALUATE(cretenciones+".Importe")
       CASE ALLTRIM(EVALUATE(cretenciones+".impuesto"))="IVA"
          UPDATE (cgenerales) SET retiva = "IVA", totretiva = EVALUATE(cretenciones+".Importe")
    ENDCASE
 ENDSCAN
 SELECT (cgenerales)
 SELECT (ctraslados)
 leerdatos(xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Impuestos/"+lcprefijo+"Traslados/"+lcprefijo+"Traslado"), 'IT')
 SCAN
    DO CASE
       CASE ALLTRIM(EVALUATE(ctraslados+".Impuesto"))="IVA"
          UPDATE (cgenerales) SET tipofactor = 'Tasa', impuesto = "IVA", tasaocuota = EVALUATE(ctraslados+".TasaOCuota"), importe = EVALUATE(ctraslados+".Importe")
       CASE ALLTRIM(EVALUATE(ctraslados+".Impuesto"))="IEPS"
          UPDATE (cgenerales) SET traieps = "IEPS", tratasieps = EVALUATE(ctraslados+".TasaOCuota"), tottraieps = EVALUATE(ctraslados+".Importe")
    ENDCASE
 ENDSCAN
 SELECT (cgenerales)
 olnodes = xdoc.selectnodes("//"+lcprefijo+"Comprobante/"+lcprefijo+"Complemento/"+lcprefijo+"TimbreFiscalDigital/")
 FOR i = 0 TO olnodes.length-1
    FOR j = 0 TO olnodes.item(i).attributes.length-1
       satributename = olnodes.item(i).attributes.item(j).nodename
       satributevalues = olnodes.item(i).attributes.item(j).text
       DO CASE
          CASE satributename="Version"
             UPDATE (cgenerales) SET versiontfd = satributevalues
          CASE satributename="FechaTimbrado"
             UPDATE (cgenerales) SET fechatfd = satributevalues
          CASE satributename="SelloCFD"
             UPDATE (cgenerales) SET sellocfd = satributevalues
          CASE satributename="NoCertificadoSAT"
             UPDATE (cgenerales) SET certsat = satributevalues
          CASE satributename="SelloSAT"
             UPDATE (cgenerales) SET sellosat = satributevalues
          CASE satributename="UUID"
             UPDATE (cgenerales) SET uuid = satributevalues
       ENDCASE
    ENDFOR
 ENDFOR
 SELECT (cgenerales)
 LOCAL stroriginal
 stroriginal = cfdextraercadenaoriginal(archivoxml, pcopenssl)
 UPDATE (cgenerales) SET cadenaorig = stroriginal
 RETURN
ENDFUNC
**
PROCEDURE LeerDatos
 LPARAMETERS root, sroot
 LOCAL child
 FOR EACH child IN root
    DO CASE
       CASE child.nodename==lcprefijo+"Concepto"
          INSERT INTO (cconceptos) (cantidad) VALUES (0)
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             DO CASE
                CASE satributename="ClaveProdServ"
                   REPLACE claveprodserv WITH satributevalues IN (cconceptos)
                CASE satributename="NoIdentificacion"
                   REPLACE noidentificacion WITH satributevalues IN (cconceptos)
                CASE satributename="Cantidad"
                   REPLACE cantidad WITH VAL(satributevalues) IN (cconceptos)
                CASE satributename="ClaveUnidad"
                   REPLACE claveunidad WITH satributevalues IN (cconceptos)
                CASE satributename="Unidad"
                   REPLACE unidad WITH satributevalues IN (cconceptos)
                CASE satributename="Descripcion"
                   REPLACE descripcio WITH satributevalues IN (cconceptos)
                CASE satributename="ValorUnitario"
                   REPLACE valorunita WITH VAL(satributevalues) IN (cconceptos)
                CASE satributename="Importe"
                   REPLACE importe WITH VAL(satributevalues) IN (cconceptos)
                CASE satributename="Descuento"
                   REPLACE dscnt WITH VAL(satributevalues) IN (cconceptos)
                CASE satributename="Impuesto"
                   REPLACE impuesto WITH satributevalues IN (cconceptos)
                OTHERWISE
                   WAIT WINDOW 'Atributo no encontrado: '+satributename
             ENDCASE
          ENDFOR
       CASE child.nodename==lcprefijo+"Traslado"
          IF sroot<>'IT'
             FOR j = 0 TO child.attributes.length-1
                satributename = child.attributes.item(j).nodename
                satributevalues = child.attributes.item(j).text
                DO CASE
                   CASE satributename="Impuesto"
                      REPLACE impuesto WITH satributevalues IN (cconceptos)
                   CASE satributename="TipoFactor"
                      REPLACE tipofactor WITH satributevalues IN (cconceptos)
                   CASE satributename="TasaOCuota"
                      REPLACE tasaocuota WITH VAL(satributevalues) IN (cconceptos)
                   CASE satributename="Importe"
                      REPLACE iimporte WITH VAL(satributevalues) IN (cconceptos)
                ENDCASE
             ENDFOR
          ELSE
             INSERT INTO (ctraslados) (impuesto) VALUES (" ")
             FOR j = 0 TO child.attributes.length-1
                satributename = child.attributes.item(j).nodename
                satributevalues = child.attributes.item(j).text
                DO CASE
                   CASE satributename="impuesto"
                      REPLACE impuesto WITH satributevalues IN (ctraslados)
                   CASE satributename="importe"
                      REPLACE importe WITH VAL(satributevalues) IN (ctraslados)
                   CASE satributename="tasa"
                      REPLACE tasa WITH VAL(satributevalues) IN (ctraslados)
                ENDCASE
             ENDFOR
          ENDIF
       CASE child.nodename==lcprefijo+"Retencion"
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             DO CASE
                CASE satributename="Impuesto"
                   REPLACE impuesto WITH satributevalues IN (cconceptos)
                CASE satributename="TipoFactor"
                   REPLACE tipofactor WITH satributevalues IN (cconceptos)
                CASE satributename="TasaOCuota"
                   REPLACE tasaocuota WITH VAL(satributevalues) IN (cconceptos)
                CASE satributename="Importe"
                   REPLACE iimporte WITH VAL(satributevalues) IN (cconceptos)
             ENDCASE
          ENDFOR
       CASE child.nodename==lcprefijo+"CuentaPredial"
          SELECT (cconceptos)
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             DO CASE
                CASE satributename="numero"
                   REPLACE nopredio WITH satributevalues
             ENDCASE
          ENDFOR
       CASE child.nodename==lcprefijo+"RegimenFiscal"
          INSERT INTO (cregimenes) (regimen) VALUES (" ")
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             IF satributename="Regimen"
                REPLACE regimen WITH satributevalues IN (cregimenes)
             ENDIF
          ENDFOR
       CASE child.nodename==lcprefijo+"InformacionAduanera"
          SELECT (cconceptos)
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             DO CASE
                CASE satributename="numero"
                   REPLACE ianumero WITH satributevalues
                CASE satributename="fecha"
                   REPLACE iafecha WITH satributevalues
                CASE satributename="aduana"
                   REPLACE iaaduana WITH satributevalues
             ENDCASE
          ENDFOR
          LOCAL oia
          SCATTER NAME oia
          INSERT INTO (caduanas) VALUES (oia.noid, oia.ianumero, oia.iafecha, oia.iaaduana)
       CASE child.nodename=="iedu:instEducativas"
          SELECT (cconceptos)
          FOR j = 0 TO child.attributes.length-1
             satributename = child.attributes.item(j).nodename
             satributevalues = child.attributes.item(j).text
             DO CASE
                CASE satributename="nombreAlumno"
                   REPLACE iedunomal WITH satributevalues IN (cconceptos)
                CASE satributename="CURP"
                   REPLACE ieducurp WITH satributevalues IN (cconceptos)
                CASE satributename="nivelEducativo"
                   REPLACE iedunived WITH satributevalues IN (cconceptos)
                CASE satributename="autRVOE"
                   REPLACE iedurvoe WITH satributevalues IN (cconceptos)
                CASE satributename="rfcPago"
                   REPLACE iedurfcpa WITH satributevalues IN (cconceptos)
             ENDCASE
          ENDFOR
    ENDCASE
    IF child.haschildnodes
       leerdatos(child.childnodes, '')
    ENDIF
 ENDFOR
ENDPROC
**
PROCEDURE CreaFactura
 swerror = .T.
 CREATE CURSOR (cgenerales) (versions C (005), serie C (020), folio C (010), fecha C (020), noaprobaci C (014), anoaprobac C (004), formapago C (080), usocfdi C (250), condicione C (250), metododepa C (250), motivodesc C (250), subtotal N (10, 6), descuento N (10, 6), total N (10, 6), tipodecomp C (010), nocertific C (030), sello M, certific M, cadenaorig M, enombre C (150), erfc C (013), eregimenfiscal C (003), ecalle C (100), ecodigopos C (010), ecolonia C (100), ereferen C (250), eestado C (050), elocalidad C (050), emunicipio C (050), enoexterio C (050), enointerio C (050), epais C (010), xcalle C (100), xcodigopos C (010), xcolonia C (100), xestado C (050), xlocalidad C (050), xreferen C (250), xmunicipio C (050), xnoexterio C (050), xnointerio C (050), xpais C (010), rnombre C (150), rrfc C (013), rcalle C (100), rcodigopos C (010), rcolonia C (100), rreferen C (250), restado C (050), rlocalidad C (050), rmunicipio C (050), rnoexterio C (050), rnointerio C (050), rpais C (010), totimptras N (10, 6), retisr C (010), totretisr N (10, 6), retiva C (010), totretiva N (10, 6), totimpret N (10, 6), base N (10, 6), impuesto C (010), tipofactor C (006), tasaocuota N (10, 6), importe N (10, 6), traieps C (004), tratasieps N (10, 6), tottraieps N (10, 6), referencia C (010), tipocambio C (013), moneda C (020), cliente C (010), pedido C (010), agente C (010), embarque C (020), proveedor C (010), ordencompr C (010), rtelefono C (030), etelefono C (030), eweb C (050), email C (050), totalletra C (250), notas C (250), fechacfd C (050), fechavence C (050), versiontfd C (010), fechatfd C (020), sellocfd M, sellosat M, certsat C (030), uuid C (036), mfoliofo N (10, 6), ffoliofo C (20), sfoliofo C (20), foliofo C (10), numctapago C (250), lugarexp C (250), cveproser C (008), noidentificacion C (020), cveunidad C (003), descripcion C (250), valorunitario N (10, 6), regsfis C (100), banda N (1))
 CREATE CURSOR (cconceptos) (claveprodserv C (008), cantidad N (10, 3), claveunidad C (003), noidentificacion C (020), descripcio M, noid C (020), infoaduana C (250), importe N (10, 6), unidad C (010), valorunita N (10, 6), dscnt N (10, 6), nopredio C (050), ianumero C (050), iafecha C (010), iaaduana C (050), iedunomal C (080), ieducurp C (018), iedunived C (040), iedurvoe C (040), iedurfcpa C (013), base N (15, 2), impuesto C (004), tipofactor C (006), tasaocuota N (10, 6), iimporte N (15, 2), banda N (1))
 CREATE CURSOR (cretenciones) (impuesto C (004), importe N (10, 6))
 CREATE CURSOR (ctraslados) (impuesto C (004), tasa N (10, 6), importe N (10, 6))
 CREATE CURSOR (cctraslados) (base N (10, 6), impuesto C (004), tipofactor C (006), tasaocuota N (10, 6), importe N (10, 6))
 CREATE CURSOR (ccretenciones) (base N (10, 6), impuesto C (004), tipofactor C (006), tasaocuota N (10, 6), importe N (10, 6))
 CREATE CURSOR (caduanas) (noid C (020), numero C (020), fecha C (010), aduana C (020))
 CREATE CURSOR (cregimenes) (regimen C (250))
 swerror = .F.
 RETURN
ENDPROC
**
FUNCTION CFDDomicilio
 LPARAMETERS pccalle, pcnoexterior, pcnointerior, pcreferencia, pccolonia, pclocalidad, pcmunicipio, pcestado, pccodpostal, pcpais
 LOCAL cdireccion
 cdireccion = ALLTRIM(pccalle)+" "+ALLTRIM(pcnoexterior)+" "+ALLTRIM(pcnointerior)+","+ALLTRIM(pcreferencia)+","+IIF(EMPTY(pccolonia), "", "Col. "+ALLTRIM(pccolonia))+","+IIF(EMPTY(pccodpostal), "", "CP "+ALLTRIM(pccodpostal))+","+ALLTRIM(pclocalidad)+","+ALLTRIM(pcmunicipio)+","+ALLTRIM(pcestado)+","+ALLTRIM(pcpais)
 cdireccion = ALLTRIM(cdireccion)
 DO WHILE AT(SPACE(2), cdireccion)>0
    cdireccion = STRTRAN(cdireccion, SPACE(2), SPACE(1))
 ENDDO
 DO WHILE AT(" ,", cdireccion)>0
    cdireccion = STRTRAN(cdireccion, " ,", ",")
 ENDDO
 DO WHILE AT(",,", cdireccion)>0
    cdireccion = STRTRAN(cdireccion, ",,", ",")
 ENDDO
 IF LEFT(cdireccion, 1)=","
    SUBSTR(cdireccion, 2)
 ENDIF
 cdireccion = STRTRAN(cdireccion, ",", ", ")
 RETURN cdireccion
ENDFUNC
**
FUNCTION CFDPrint
 LPARAMETERS pcxml, plpreview, plpdfmode, pcpdftarget, plreplaceco
 IF  .NOT. cfdtocursor(pcxml)
    RETURN .F.
 ENDIF
 SELECT qdg
 REPLACE regsfis WITH m.regsfis IN qdg
 SELECT qrf
 REPLACE regsfis WITH m.regsfis IN qdg
 SCAN
    REPLACE regsfis WITH m.regsfis IN qdg
 ENDSCAN
 IF plreplaceco
    SELECT cantidad, codigo AS noidentificacion, descripcion AS descripcio, "" AS infoaduana, importe, dscnt, unidad, precio_unit AS valorunita, cuentapredial AS nopredio, cveproser, cveunidad, base, impuesto, tipofactor, tasaocuota, iimporte, banda, regsfis FROM curConceptos INTO CURSOR QCOTEMP
    SELECT 0
    USE (DBF("QCOTEMP")) AGAIN ALIAS qco
    USE IN qcotemp
 ENDIF
 UPDATE QCO SET banda = 1
 INSERT INTO QCO (banda, descripcio, valorunita, importe) VALUES (2, "Sub / Total", 0.00 , qdg.subtotal)
 IF qdg.descuento>0
    INSERT INTO QCO (banda, descripcio, valorunita, importe) VALUES (2, qdg.motivodesc, 0.00 , -qdg.descuento)
 ENDIF
 LOCAL cimpuesto, ctipofactor, ntasaocuota, nimporte
 SELECT qtr
 GOTO TOP
 SCAN
    cimpuesto = impuesto
    ntasaocuota = tasa
    nimporte = importe
    INSERT INTO QCO (banda, descripcio, valorunita, importe) VALUES (3, cimpuesto, ntasaocuota, nimporte)
    SELECT qtr
 ENDSCAN
 LOCAL cimpuestor, nimporter
 SELECT qre
 GOTO TOP
 SCAN
    cimpuestor = "RET. "+ALLTRIM(impuesto)
    nimporter = importe
    INSERT INTO QCO (banda, descripcio, valorunita, importe) VALUES (4, cimpuestor, 0.00 , nimporter)
    SELECT qre
 ENDSCAN
 INSERT INTO QCO (banda, descripcio, valorunita, importe) VALUES (5, "Total", 0.00 , qdg.total)
 SELECT qco
 GOTO TOP
 LOCAL cformat, lresult
 cformat = ALLTRIM(cfdconf.formatoimpresion)
 lresult = .T.
 DO CASE
    CASE plpreview
       REPORT FORM (cformat) NOCONSOLE PREVIEW
    CASE plpdfmode .AND. cfdconf.usarprint2pdf
       print2pdf(pcpdftarget, cformat)
       lresult = FILE(pcpdftarget)
    CASE plpdfmode .AND.  .NOT. cfdconf.usarprint2pdf
       pcpdftarget = FULLPATH(pcpdftarget)
       LOCAL opdf
       opdf = CREATEOBJECT("PDFCreator.clsPDFCreator")
       opdf.cstart()
       opdf.cvisible = .F.
       opdf.cclearcache()
       opdf.cprinterstop = .F.
       opdf.coption("AutosaveDirectory") = JUSTPATH(pcpdftarget)
       opdf.coption("AutosaveFileName") = JUSTFNAME(pcpdftarget)
       opdf.coption("UseAUtosave") = 1
       opdf.coption("UseAutosaveDirectory") = 1
       opdf.coption("AutosaveFormat") = 0
       opdf.csaveoptions()
       SET PRINTER TO NAME PDFCreator
       REPORT FORM (cformat) TO PRINTER NOCONSOLE
       SET PRINTER TO
       sleep(3000)
       opdf.coption("UseAUtosave") = 0
       opdf.csaveoptions()
       lresult = FILE(pcpdftarget)
    OTHERWISE
       REPORT FORM (cformat) TO PRINTER PROMPT NOCONSOLE PREVIEW IN screen
 ENDCASE
 USE IN qdg
 USE IN qco
 USE IN qre
 USE IN qtr
 USE IN qad
 RETURN lresult
ENDFUNC
**
FUNCTION CFDNTOCESP
 LPARAMETERS pnnumero, pcmoneda, pcprefijo, pcsufijo
 pcmoneda = IIF(VARTYPE(pcmoneda)="C", pcmoneda, cfdconf.ntocmoneda)
 pcprefijo = IIF(VARTYPE(pcprefijo)="C", pcprefijo, cfdconf.ntocprefijo)
 pcsufijo = IIF(VARTYPE(pcsufijo)="C", pcsufijo, cfdconf.ntocsufijo)
 DIMENSION afactores[4], acentenas[10], adecenas[10], aunidades[10], aespeciales[5]
 afactores[1] = ""
 afactores[2] = "MIL"
 afactores[3] = "MILLON"
 afactores[4] = "MIL MILLONES"
 acentenas[1] = ""
 acentenas[2] = "CIENTO"
 acentenas[3] = "DOSCIENTOS"
 acentenas[4] = "TRESCIENTOS"
 acentenas[5] = "CUATROCIENTOS"
 acentenas[6] = "QUINIENTOS"
 acentenas[7] = "SEISCIENTOS"
 acentenas[8] = "SETECIENTOS"
 acentenas[9] = "OCHOCIENTOS"
 acentenas[10] = "NOVECIENTOS"
 adecenas[1] = ""
 adecenas[2] = "DIEZ"
 adecenas[3] = "VEINTE"
 adecenas[4] = "TREINTA"
 adecenas[5] = "CUARENTA"
 adecenas[6] = "CINCUENTA"
 adecenas[7] = "SESENTA"
 adecenas[8] = "SETENTA"
 adecenas[9] = "OCHENTA"
 adecenas[10] = "NOVENTA"
 aunidades[1] = ""
 aunidades[2] = "UNO"
 aunidades[3] = "DOS"
 aunidades[4] = "TRES"
 aunidades[5] = "CUATRO"
 aunidades[6] = "CINCO"
 aunidades[7] = "SEIS"
 aunidades[8] = "SIETE"
 aunidades[9] = "OCHO"
 aunidades[10] = "NUEVE"
 aespeciales[1] = "ONCE"
 aespeciales[2] = "DOCE"
 aespeciales[3] = "TRECE"
 aespeciales[4] = "CATORCE"
 aespeciales[5] = "QUINCE"
 LOCAL ccifra1, cletras, nfact, ntercio, ncount, nfactor, ncentana, ndecena, nunidad
 ccifra1 = ","+LTRIM(TRANSFORM(INT(pnnumero), "999,999,999,999,999,999"))
 ncount = OCCURS(",", ccifra1)
 cletras = ""
 FOR ntercio = 1 TO ncount
    ctercio = SUBSTR(ccifra1, RAT(",", ccifra1)+1, 3)
    ccifra1 = SUBSTR(ccifra1, 1, RAT(",", ccifra1)-1)
    nbase = INT(VAL(ctercio))
    ncentena = INT(nbase/100)
    nbase = nbase-(INT(nbase/100)*100)
    ndecena = INT(nbase/10)
    nunidad = nbase-(INT(nbase/10)*10)
    cfactor = afactores(ntercio)
    IF ntercio=3 .AND. (nbase>1 .OR. ndecena>0 .OR. ncentena>0)
       cfactor = "MILLONES"
    ENDIF
    ccentena = acentenas(ncentena+1)
    IF ndecena>1 .OR. nunidad=0
       cdecena = adecenas(ndecena+1)
       cunidad = aunidades(nunidad+1)
    ELSE
       IF ndecena=1 .AND. BETWEEN(nunidad, 1, 5)
          cdecena = aespeciales(nunidad)
          cunidad = ""
       ELSE
          cdecena = IIF(ndecena=1, "DIEZ", "")
          cunidad = aunidades(nunidad+1)
       ENDIF
    ENDIF
    DO CASE
       CASE ncentena>0 .AND. ndecena=0 .AND. nunidad=0
          IF ncentena=1
             ctercio = "CIEN "
          ELSE
             ctercio = ccentena+" "
          ENDIF
       CASE ncentena=0 .AND. ndecena=0
          IF nunidad=1
             IF  .NOT. EMPTY(cfactor)
                ctercio = "UN "
             ELSE
                ctercio = "UNO "
             ENDIF
          ELSE
             ctercio = cunidad+" "
          ENDIF
       OTHERWISE
          ctercio = IIF( .NOT. EMPTY(ccentena), ccentena+" ", "")+IIF( .NOT. EMPTY(cdecena), cdecena+" ", "")+IIF( .NOT. EMPTY(cunidad), IIF(EMPTY(cdecena), cunidad, "Y "+cunidad)+" ", "")
    ENDCASE
    IF  .NOT. EMPTY(ctercio)
       cletras = ctercio+cfactor+" "+cletras
    ENDIF
 ENDFOR
 LOCAL ndec
 ndec = (pnnumero-INT(pnnumero))*100
 cletras = pcprefijo+" "+PROPER(RTRIM(cletras))+" "+pcmoneda+" con "+PADL(INT(ndec), 2, "0")+"/100 "+pcsufijo
 cletras = ALLTRIM(STRTRAN(cletras, "  ", " "))
 RETURN cletras
ENDFUNC
**
FUNCTION CFDExtraerCadenaOriginal
 LPARAMETERS pcxml, pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcxml) .OR.  .NOT. FILE(pcxml)
    cfdconf.ultimoerror = "El archivo indicado no existe"
    RETURN ""
 ENDIF
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN ""
 ENDIF
 pcopenssl = ADDBS(FULLPATH(pcopenssl))
 LOCAL cbatfile, ccadfile, cxslfile, cxsltproc, cbuff
 cbatfile = gettempfile("BAT")
 ccadfile = FORCEEXT(cbatfile, "CAD")
 cxslfile = FORCEEXT(cbatfile, "XSLT")
 cxsltproc = getshortname(pcopenssl+"XSLTPROC.EXE")
 LOCAL cbuff, nxmlversion
 cbuff = FILETOSTR(pcxml)
 cbuff = SUBSTR(cbuff, AT("Comprobante ", cbuff))
 cbuff = LEFT(cbuff, AT(">", cbuff))
 cbuff = SUBSTR(cbuff, ATC("Version", cbuff))
 cbuff = LEFT(cbuff, AT(" ", cbuff)-1)
 cbuff = CHRTRAN(cbuff, 'Version="', "")
 nxmlversion = cfdversions.fromstring(cbuff)
 DO CASE
    CASE nxmlversion=cfdversions.cfd_20
       cxsltfile = "cadenaoriginal_2_0_local.xslt"
    CASE nxmlversion=cfdversions.cfdi_30
       cxsltfile = "cadenaoriginal_3_0_local.xslt"
    CASE nxmlversion=cfdversions.cfd_22
       cxsltfile = "cadenaoriginal_2_2_local.xslt"
    CASE nxmlversion=cfdversions.cfdi_32
       cxsltfile = "cadenaoriginal_3_2_local.xslt"
    CASE nxmlversion=cfdversions.cfdi_33
       cxsltfile = "cadenaoriginal_3_3_local.xslt"
    OTHERWISE
       WAIT WINDOW 'No se encontreo versión de cfdi'
       RETURN ''
 ENDCASE
 cbuff = FILETOSTR(pcopenssl+cxsltfile)
 cbuff = STRTRAN(cbuff, "<ssl-path>", CHRTRAN(pcopenssl, "\", "/"))
 STRTOFILE(cbuff, cxslfile)
 cbuff = cxsltproc+" {xsltFile} {xmlFile} > {cadFile}"+CHR(13)+CHR(10)
 cbuff = STRTRAN(cbuff, "{xsltFile}", getshortname(FULLPATH(cxslfile)))
 cbuff = STRTRAN(cbuff, "{xmlFile}", getshortname(FULLPATH(pcxml)))
 cbuff = STRTRAN(cbuff, "{cadFile}", ccadfile)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 LOCAL ccadenaoriginal
 ccadenaoriginal = ""
 IF FILE(ccadfile)
    ccadenaoriginal = cfdutf82asc(FILETOSTR(ccadfile))
 ELSE
    cfdconf.ultimoerror = "Ocurrio un error inesperado al obtener la cadena original"
    ccadenaoriginal = ""
 ENDIF
 ERASE (ccadfile)
 IF  .NOT. EMPTY(ccadenaoriginal)
    ERASE (cbatfile)
    ERASE (cxslfile)
 ENDIF
 RETURN ccadenaoriginal
ENDFUNC
**
PROCEDURE CFDEnviarPorCorreo
 LPARAMETERS pcdestinatario, pcasunto, pccuerpo, pccfd, pladjuntarpdf
 LOCAL lcschema, loconfig, lomsg
 lcschema = "http://schemas.microsoft.com/cdo/configuration/"
 loconfig = CREATEOBJECT("CDO.Configuration")
 WITH loconfig.fields
    .item(lcschema+"smtpserver") = cfdconf.smtpserver
    .item(lcschema+"smtpserverport") = cfdconf.smtpport
    .item(lcschema+"sendusing") = 2
    .item(lcschema+"smtpauthenticate") = .T.
    .item(lcschema+"smtpusessl") = cfdconf.smtpusessl
    .item(lcschema+"sendusername") = cfdconf.smtpusername
    .item(lcschema+"sendpassword") = cfdconf.smtppassword
    .update()
 ENDWITH
 lomsg = CREATEOBJECT("CDO.Message")
 WITH lomsg
    .configuration = loconfig
    .from = cfdconf.mailsender
    .to = pcdestinatario
    .subject = pcasunto
    .htmlbody = pccuerpo
    .addattachment(FULLPATH(pccfd))
 ENDWITH
 IF pladjuntarpdf
    LOCAL cpdf
    cpdf = FORCEEXT(pccfd, "PDF")
    cfdprint(pccfd, , .T., cpdf)
    lomsg.addattachment(FULLPATH(cpdf))
 ENDIF
 lomsg.send()
 lomsg = .NULL.
 loconfig = .NULL.
ENDPROC
**
PROCEDURE CFDEnviarPorCorreoAdjuntos
 LPARAMETERS pcdestinatario, pcasunto, pccuerpo, pccfd, pcpdf
 LOCAL lcschema, loconfig, lomsg
 lcschema = "http://schemas.microsoft.com/cdo/configuration/"
 loconfig = CREATEOBJECT("CDO.Configuration")
 WITH loconfig.fields
    .item(lcschema+"smtpserver") = cfdconf.smtpserver
    .item(lcschema+"smtpserverport") = cfdconf.smtpport
    .item(lcschema+"sendusing") = 2
    .item(lcschema+"smtpauthenticate") = .T.
    .item(lcschema+"smtpusessl") = cfdconf.smtpusessl
    .item(lcschema+"sendusername") = cfdconf.smtpusername
    .item(lcschema+"sendpassword") = cfdconf.smtppassword
    .update()
 ENDWITH
 lomsg = CREATEOBJECT("CDO.Message")
 WITH lomsg
    .configuration = loconfig
    .from = cfdconf.mailsender
    .to = pcdestinatario
    .subject = pcasunto
    .htmlbody = pccuerpo
    IF FILE(FULLPATH(pccfd))
       .addattachment(FULLPATH(pccfd))
    ENDIF
    IF FILE(FULLPATH(pcpdf))
       .addattachment(FULLPATH(pcpdf))
    ENDIF
 ENDWITH
 lomsg.send()
 lomsg = .NULL.
 loconfig = .NULL.
ENDPROC
**
FUNCTION GetShortName
 LPARAMETERS pclongpath
 IF  .NOT. FILE(pclongpath)
    MESSAGEBOX("No se pudo encontrar el archivo:"+REPLICATE(CHR(13)+CHR(10), 2)+LOWER(pclongpath), 16, "GetShortName")
    RETURN pclongpath
 ENDIF
 LOCAL ofso, ofi, cshortpath
 ofso = CREATEOBJECT("Scripting.FileSystemObject")
 ofi = ofso.getfile(pclongpath)
 cshortpath = ofi.shortpath
 ofi = .NULL.
 ofso = .NULL.
 RETURN cshortpath
ENDFUNC
**
FUNCTION GetTempFile
 LPARAMETERS pcext
 IF PCOUNT()=0
    pcext = "TMP"
 ENDIF
 LOCAL ctempfile
 DO WHILE .T.
    ctempfile = FULLPATH(LEFT(SYS(2015), 8)+"."+pcext)
    IF  .NOT. FILE(ctempfile)
       STRTOFILE("", ctempfile)
       EXIT
    ENDIF
 ENDDO
 RETURN getshortname(ctempfile)
ENDFUNC
**
FUNCTION CFDLeerCertificado
 LPARAMETERS pcarchivocer, pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN .NULL.
 ENDIF
 pcopenssl = getshortname(ADDBS(FULLPATH(pcopenssl))+"OPENSSL.EXE")
 LOCAL cbatfile, ctempfile
 cbatfile = gettempfile("BAT")
 ctempfile = JUSTSTEM(cbatfile)
 cbuff = pcopenssl+" x509 -inform DER -outform PEM -in {cerFile} -pubkey > {tempFile}.pem "+CHR(13)+CHR(10)+pcopenssl+" x509 -in {tempFile}.pem -serial -noout > {tempFile}.ser"+CHR(13)+CHR(10)+pcopenssl+" x509 -inform DER -in {cerFile} -noout -startdate > {tempFile}.sta"+CHR(13)+CHR(10)+pcopenssl+" x509 -inform DER -in {cerFile} -noout -enddate > {tempFile}.end"+CHR(13)+CHR(10)
 cbuff = STRTRAN(cbuff, "{cerFile}", getshortname(FULLPATH(pcarchivocer)))
 cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 IF  .NOT. FILE(ctempfile+".PEM") .OR.  .NOT. FILE(ctempfile+".SER") .OR.  .NOT. FILE(ctempfile+".STA") .OR.  .NOT. FILE(ctempfile+".END")
    cfdconf.ultimoerror = "Ocurrio un error al intentar leer el certificado indicado"
    ERASE (ctempfile+".*")
    RETURN .NULL.
 ENDIF
 LOCAL odata
 odata = CREATEOBJECT("CFDCertificado")
 odata.archivo = ALLTRIM(LOWER(pcarchivocer))
 odata.valido = .F.
 odata.vigente = .F.
 odata.certificado = ""
 odata.serial = ""
 odata.vigentedesde = { :}
 odata.vigentehasta = { :}
 LOCAL ccert
 ccert = FILETOSTR(ctempfile+".PEM")
 ccert = SUBSTR(ccert, AT("-----BEGIN CERTIFICATE-----", ccert))
 ccert = STRTRAN(ccert, "-----BEGIN CERTIFICATE-----", "")
 ccert = STRTRAN(ccert, "-----END CERTIFICATE-----", "")
 ccert = STRTRAN(ccert, CHR(13))
 ccert = STRTRAN(ccert, CHR(10))
 odata.certificado = ccert
 LOCAL cserie
 cserie = FILETOSTR(ctempfile+".SER")
 cserie = STRTRAN(cserie, "serial=", "")
 cserie = STRTRAN(cserie, CHR(10), "")
 cserie = hex2str(cserie)
 odata.serial = cserie
 odata.vigentedesde = fctot(ctempfile+".STA")
 odata.vigentehasta = fctot(ctempfile+".END")
 odata.valido = ( .NOT. EMPTY(odata.certificado))
 odata.vigente = BETWEEN(DATETIME(), odata.vigentedesde, odata.vigentehasta)
 ERASE (ctempfile+".*")
 RETURN odata
ENDFUNC
**
FUNCTION CFDVigenciaCert
 LPARAMETERS pccer, ptfechacomp, pcopenssl
 LOCAL ocert
 ocert = cfdleercertificado(pccer, pcopenssl)
 IF ISNULL(ocert)
    RETURN .F.
 ENDIF
 cfdconf.ultimoerror = ""
 LOCAL lresult
 lresult = .T.
 IF VARTYPE(ptfechacomp)="D"
    ptfechacomp = DTOT(ptfechacomp)
 ENDIF
 IF ptfechacomp<ocert.vigentedesde
    cfdconf.ultimoerror = "La fecha del comprobante es anterior a la vigencia del certificado."
    lresult = .F.
 ENDIF
 IF ptfechacomp>ocert.vigentehasta
    cfdconf.ultimoerror = "La fecha del comprobante es posterior a la vigencia del certificado."
    lresult = .F.
 ENDIF
 RETURN lresult
ENDFUNC
**
FUNCTION FCTOT
 LPARAMETERS pcarchivo
 LOCAL cdata, cyy, cmm, cdd, ctt
 cdata = CHRTRAN(FILETOSTR(pcarchivo), CHR(13)+CHR(10), "")
 cdata = SUBSTR(cdata, AT("=", cdata)+1)
 cdata = ALLTRIM(STRTRAN(cdata, "GMT", ""))
 cyy = RIGHT(cdata, 4)
 cmm = UPPER(LEFT(cdata, 3))
 cmm = STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(cmm, "JAN", "01"), "FEB", "02"), "MAR", "03"), "APR", "04"), "MAY", "05"), "JUN", "06")
 cmm = STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(cmm, "JUL", "07"), "AUG", "08"), "SEP", "09"), "OCT", "10"), "NOV", "11"), "DEC", "12")
 cdd = ALLTRIM(SUBSTR(cdata, 5))
 cdd = PADL(LEFT(cdd, AT(" ", cdd)-1), 2, "0")
 ctt = LEFT(SUBSTR(cdata, AT(":", cdata)-2), 8)
 LOCAL csetdate, tfechahora
 csetdate = SET("DATE")
 SET DATE YMD
 tfechahora = CTOT(cyy+"-"+cmm+"-"+cdd+" "+ctt)
 IF csetdate<>"YMD"
    SET DATE (csetdate)
 ENDIF
 RETURN tfechahora
ENDFUNC
**
FUNCTION CFDValidarKeyCer
 LPARAMETERS pcarchivokey, pcarchivocer, pcpassword, pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN .F.
 ENDIF
 pcopenssl = getshortname(ADDBS(FULLPATH(pcopenssl))+"OPENSSL.EXE")
 LOCAL cbatfile, ctempfile
 cbatfile = gettempfile("BAT")
 ctempfile = JUSTSTEM(cbatfile)
 LOCAL cbuff
 cbuff = pcopenssl+" x509 -inform DER -in {cerFile} -noout -modulus > {tempFile}.m1"+CHR(13)+CHR(10)+pcopenssl+" x509 -outform der -in {cerFile} -out > {tempFile}.der"+CHR(13)+CHR(10)+pcopenssl+" pkcs8 -inform DER -in {keyFile} -passin pass:{password} -out {tempFile}.pem"+CHR(13)+CHR(10)+pcopenssl+" rsa -in {tempFile}.pem -noout -modulus > {tempFile}.m2"+CHR(13)+CHR(10)+"DEL {tempFile}.pem"+CHR(13)+CHR(10)
 cbuff = STRTRAN(cbuff, "{cerFile}", getshortname(FULLPATH(pcarchivocer)))
 cbuff = STRTRAN(cbuff, "{keyFile}", getshortname(FULLPATH(pcarchivokey)))
 cbuff = STRTRAN(cbuff, "{password}", pcpassword)
 cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 IF  .NOT. FILE(ctempfile+".M1") .OR.  .NOT. FILE(ctempfile+".M2")
    cfdconf.ultimoerror = "Ocurrio un error al intentar validar los archivos KEY y/o CER"
    ERASE (ctempfile+".*")
    RETURN .F.
 ENDIF
 LOCAL ccermod, ckeymod
 ccermod = FILETOSTR(ctempfile+".m1")
 ckeymod = FILETOSTR(ctempfile+".m2")
 IF EMPTY(ccermod)
    cfdconf.ultimoerror = "No se pudo obtener el modulus del archivo CER"
    ERASE (ctempfile+".*")
    RETURN .F.
 ENDIF
 IF EMPTY(ckeymod)
    cfdconf.ultimoerror = "No se pudo obtener el modulus del archivo KEY (verifique la contraseña)"
    ERASE (ctempfile+".*")
    RETURN .F.
 ENDIF
 LOCAL lvalid
 lvalid = (ccermod==ckeymod)
 IF  .NOT. lvalid
    cfdconf.ultimoerror = "El archivo KEY no corresponde con el archivo CER indicado"
 ENDIF
 ERASE (ctempfile+".*")
 RETURN lvalid
ENDFUNC
**
FUNCTION CFDValidarXML
 LPARAMETERS pcarchivoxml, pcarchivokey, pcpassword, pcmetodo, pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN .F.
 ENDIF
 pcopenssl = getshortname(ADDBS(FULLPATH(pcopenssl))+"OPENSSL.EXE")
 LOCAL oxml
 oxml = CREATEOBJECT('MSXML2.DOMdocument')
 oxml.load(getshortname(pcarchivoxml))
 IF oxml.parseerror.errorcode<>0
    cfdconf.ultimoerror = "Estructura XML mal armada. "+oxml.parseerror.reason
    RETURN .F.
 ENDIF
 orootnode = oxml.documentelement
 croottagname = orootnode.tagname
 olnode = oxml.selectsinglenode("//"+croottagname)
 IF ISNULL(olnode)
    MESSAGEBOX("XML inválido."+CHR(13)+"Nodo //"+croottagname+" no presente.", 16, "Sistema")
    lresult = .F.
    RETURN .F.
 ENDIF
 satributevalue = olnode.getattribute("Version")
 DO CASE
    CASE satributevalue="2.0"
       lcxsdfilev = "cfdv2.xsd"
       lcurlxsdv = "http://www.sat.gob.mx/sitio_internet/cfd/2/cfdv2.xsd"
       lcurlxnsv = "http://www.sat.gob.mx/cfd/2"
    CASE satributevalue="3.0"
       lcxsdfilev = "cfdv3.xsd"
       lcurlxsdv = "http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv3.xsd"
       lcurlxnsv = "http://www.sat.gob.mx/cfd/3"
    CASE satributevalue="2.2"
       lcxsdfilev = "cfdv22.xsd"
       lcurlxsdv = "http://www.sat.gob.mx/sitio_internet/cfd/2/cfdv22.xsd"
       lcurlxnsv = "http://www.sat.gob.mx/cfd/2"
    CASE satributevalue="3.2"
       lcxsdfilev = "cfdv32.xsd"
       lcurlxsdv = "http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv32.xsd"
       lcurlxnsv = "http://www.sat.gob.mx/cfd/3"
    CASE satributevalue="3.3"
       lcxsdfilev = "cfdv33.xsd"
       lcurlxsdv = "http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd"
       lcurlxnsv = "http://www.sat.gob.mx/cfd/3"
 ENDCASE
 IF  .NOT. FILE(lcxsdfilev)
    STRTOFILE(geturl(lcurlxsdv), lcxsdfilev)
 ENDIF
 LOCAL lresult
 lresult = .F.
 DO CASE
    CASE FILE(ADDBS(JUSTPATH(pcopenssl))+"CFDVALIDATOR.EXE") .AND. FILE(ADDBS(JUSTPATH(pcopenssl))+"Interop.MSXML2.dll")
       LOCAL cbatfile, ctempfile
       cbatfile = gettempfile("BAT")
       ctempfile = JUSTSTEM(cbatfile)
       LOCAL cvalidator, cbuff
       cvalidator = getshortname(ADDBS(JUSTPATH(pcopenssl))+"CFDVALIDATOR.EXE")
       cbuff = cvalidator+" -xml:{xmlFile} -xsd:{xsdFile} -xns:{xnsURL} > {tempFile}.out"+CHR(13)+CHR(10)
       cbuff = STRTRAN(cbuff, "{xmlFile}", getshortname(pcarchivoxml))
       cbuff = STRTRAN(cbuff, "{xsdFile}", getshortname(FULLPATH(lcxsdfilev)))
       cbuff = STRTRAN(cbuff, "{xnsURL}", lcurlxnsv)
       cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
       STRTOFILE(cbuff, cbatfile)
       LOCAL owsh
       owsh = CREATEOBJECT("WScript.Shell")
       owsh.run(cbatfile, 0, .T.)
       IF  .NOT. FILE(ctempfile+".OUT")
          lresult = .F.
          cfdconf.ultimoerror = "Ocurrio un error al intentar validar el XML indicado"
       ELSE
          cfdconf.ultimoerror = FILETOSTR(ctempfile+".OUT")
          cfdconf.ultimoerror = "Estructura XML mal armada. "+STRTRAN(cfdconf.ultimoerror, "[FATAL]", "[ERROR]")
          lresult = (AT("[ERROR]", cfdconf.ultimoerror)=0)
       ENDIF
       ERASE (ctempfile+".*")
    OTHERWISE
       LOCAL oxmldoc, oxmlschema
       oxmlschema = CREATEOBJECT("MSXML2.XMLSchemaCache.6.0")
       oxmlschema.validateonload = .T.
       oxmlschema.add(lcurlxnsv, FULLPATH(lcxsdfilev))
       oxmldoc = CREATEOBJECT("MSXML2.DOMDocument.6.0")
       oxmldoc.validateonparse = .T.
       oxmldoc.schemas = oxmlschema
       oxmldoc.async = .F.
       IF  .NOT. oxmldoc.load(getshortname(pcarchivoxml))
          cfdconf.ultimoerror = oxmldoc.parseerror.reason
          lresult = .F.
       ELSE
          lresult = .T.
       ENDIF
       oxmldoc = .NULL.
       oxmlschema = .NULL.
 ENDCASE
 IF lresult .AND.  .NOT. EMPTY(pcarchivokey)
    LOCAL cselloactual, ccadenaoriginal, csellocorrecto
    cselloactual = msxmlgetattribute(oxml.selectnodes("//"+croottagname), "Sello")
    ccadenaoriginal = cfdextraercadenaoriginal(pcarchivoxml, JUSTPATH(pcopenssl))
    csellocorrecto = cfdgenerarsello(ccadenaoriginal, pcarchivokey, pcpassword, pcmetodo, JUSTPATH(pcopenssl))
    IF  .NOT. cselloactual==csellocorrecto
       cfdconf.ultimoerror = cfdconf.ultimoerror+IIF(EMPTY(cfdconf.ultimoerror), "", CHR(13)+CHR(10))+"[ERROR] El sello no es valido para este certificado."+CHR(13)+CHR(10)
       _CLIPTEXT = ccadenaoriginal
       lresult = .F.
    ENDIF
 ENDIF
 RETURN lresult
ENDFUNC
**
FUNCTION GetURL
 LPARAMETERS pcurl, plverbose
 DECLARE LONG GetLastError IN WIN32API
 DECLARE INTEGER InternetCloseHandle IN "wininet.dll" LONG
 DECLARE LONG InternetOpen IN "wininet.dll" STRING, LONG, STRING, STRING, LONG
 DECLARE LONG InternetOpenUrl IN "wininet.dll" LONG, STRING, STRING, LONG, LONG, LONG
 DECLARE LONG InternetReadFile IN "wininet.dll" LONG, STRING @, LONG, LONG @
 IF plverbose
    WAIT WINDOW NOWAIT "Opening Internet connection..."
 ENDIF
 LOCAL ninethnd
 ninethnd = internetopen("GETURL", 0, "", "", 0)
 IF ninethnd=0
    RETURN ""
 ENDIF
 IF plverbose
    WAIT WINDOW NOWAIT "Opening connection to URL..."
 ENDIF
 LOCAL nurlhnd
 nurlhnd = internetopenurl(ninethnd, pcurl, .NULL., 0, 0, 0)
 IF nurlhnd=0
    internetclosehandle(ninethnd)
    RETURN ""
 ENDIF
 LOCAL curldata, cbuffer, nbytesreceived, nbuffersize
 curldata = ""
 cbuffer = ""
 nbytesreceived = 0
 nbuffersize = 0
 DO WHILE .T.
    cbuffer = REPLICATE(CHR(0), 2048)
    internetreadfile(nurlhnd, @cbuffer, LEN(cbuffer), @nbuffersize)
    IF nbuffersize=0
       EXIT
    ENDIF
    curldata = curldata+SUBSTR(cbuffer, 1, nbuffersize)
    nbytesreceived = nbytesreceived+nbuffersize
    IF plverbose
       WAIT WINDOW NOWAIT ALLTRIM(TRANSFORM(INT(nbytesreceived/1024), "999,999"))+" Kb received..."
    ENDIF
 ENDDO
 IF plverbose
    WAIT CLEAR
 ENDIF
 internetclosehandle(ninethnd)
 RETURN curldata
ENDFUNC
**
FUNCTION CFDGenerarSello
 LPARAMETERS pccadenaoriginal, pcarchivokey, pcpassword, pcmetodo, pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN ""
 ENDIF
 pcopenssl = getshortname(ADDBS(FULLPATH(pcopenssl))+"OPENSSL.EXE")
 LOCAL cbatfile, ctempfile
 cbatfile = gettempfile("BAT")
 ctempfile = JUSTSTEM(cbatfile)
 LOCAL cbuff
 cbuff = pcopenssl+" pkcs8 -inform DER -in {keyFile} -passin pass:{password} -out {tempFile}.pem"+CHR(13)+CHR(10)+pcopenssl+" dgst -{metodo} -sign {tempFile}.pem {tempFile}.cad | "+pcopenssl+" enc -base64 -A > {tempFile}.sea"+CHR(13)+CHR(10)+"del {tempFile}.cad"+CHR(13)+CHR(10)+"del {tempFile}.pem"+CHR(13)+CHR(10)
 STRTOFILE(cfdasc2utf8(pccadenaoriginal), ctempfile+".cad")
 cbuff = STRTRAN(cbuff, "{keyFile}", getshortname(pcarchivokey))
 cbuff = STRTRAN(cbuff, "{password}", pcpassword)
 cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
 cbuff = STRTRAN(cbuff, "{metodo}", pcmetodo)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 LOCAL csello
 IF FILE(ctempfile+".SEA")
    csello = FILETOSTR(ctempfile+".SEA")
 ELSE
    cfdconf.ultimoerror = "Ocurrio un error al calcular el sello digital"
    csello = ""
 ENDIF
 ERASE (ctempfile+".*")
 RETURN csello
ENDFUNC
**
FUNCTION MSXMLGetAttribute
 LPARAMETERS ponodo, pcattribute
 LOCAL ncount, i, cvalue
 ncount = ponodo.item(0).attributes.length-1
 cvalue = ""
 pcattribute = UPPER(pcattribute)
 FOR i = 0 TO ncount
    IF UPPER(ponodo.item(0).attributes.item(i).nodename)==pcattribute
       cvalue = ponodo.item(0).attributes.item(i).text
    ENDIF
 ENDFOR
 RETURN cvalue
ENDFUNC
**
DEFINE CLASS XmlPropClass AS custom
 value = "NULL"
 id = "xmlpropclass"
ENDDEFINE
**
DEFINE CLASS CFDReporteMensual AS Custom
 rfcemisor = ""
 mes = ""
 ano = ""
 registros = .NULL.
 datacursor = ""
**
   PROCEDURE Init
    this.datacursor = SYS(2015)
    SELECT 0
    CREATE CURSOR (this.datacursor) (rfc C (13), serie C (10), folio N (10), noaprob C (20), anoaprob C (4), fechahora T, fecha D, total N (15, 2), traslados N (15, 2), excento L, estado C (1), efecto C (1), pedimento C (254), fechapedi C (254), aduanaped C (254))
   ENDPROC
**
   FUNCTION Add
    LPARAMETERS pcrfc, pcserie, pnfolio, pcnoaprobacion, pcanoaprobacion, ptfecha, pntotal, pntraslados, pcestado, pcefecto, pcpedimentos, pcfechapedimentos, pcaduanapedimentos
    SELECT (this.datacursor)
    APPEND BLANK
    REPLACE rfc WITH pcrfc, serie WITH pcserie, folio WITH pnfolio, noaprob WITH pcnoaprobacion, anoaprob WITH pcanoaprobacion, fechahora WITH ptfecha, fecha WITH TTOD(ptfecha), total WITH pntotal, traslados WITH NVL(pntraslados, 0.00 ), excento WITH ISNULL(pntraslados), estado WITH IIF(VARTYPE(pnestado)<>"C", "1", pcestado), efecto WITH IIF(VARTYPE(pcefecto)<>"C", "I", pcefecto), pedimento WITH IIF(EMPTY(pcpedimentos), "", pcpedimentos), fechapedi WITH IIF(EMPTY(pcfechapedimentos), "", pcfechapedimentos), aduanaped WITH IIF(EMPTY(pcaduanapedimentos), "", pcaduanapedimentos)
    LOCAL orow
    SCATTER NAME orow
    RETURN orow
   ENDFUNC
**
   FUNCTION generarTXT
    LPARAMETERS pcdestino
    LOCAL cfilename
    cfilename = "1"+RTRIM(this.rfcemisor)+PADL(this.mes, 2, "0")+PADL(this.ano, 4, "0")+".TXT"
    IF EMPTY(pcdestino) .OR.  .NOT. DIRECTORY(pcdestino)
       pcdestino = SET("DEFAULT")+CURDIR()
    ENDIF
    cfilename = ADDBS(pcdestino)+cfilename
    LOCAL ccursor1
    ccursor1 = this.datacursor+"_Q1"
    SELECT * FROM (this.datacursor) ORDER BY serie, folio INTO CURSOR (ccursor1)
    LOCAL cbuff, i, orow, cline
    cbuff = ""
    SELECT (ccursor1)
    GOTO TOP
    SCAN
       cline = this._fixstr(rfc)+"|"+this._fixstr(serie)+"|"+this._fixstr(STR(folio))+"|"+this._fixstr(ALLTRIM(anoaprob)+ALLTRIM(noaprob))+"|"+PADL(DAY(fechahora), 2, "0")+"/"+PADL(MONTH(fechahora), 2, "0")+"/"+STR(YEAR(fechahora), 4)+" "+PADL(HOUR(fechahora), 2, "0")+":"+PADL(MINUTE(fechahora), 2, "0")+":"+PADL(SEC(fechahora), 2, "0")+"|"+this._fixstr(STR(total, 15, 2))+"|"+IIF(excento, "", this._fixstr(STR(traslados, 15, 2)))+"|"+this._fixstr(estado)+"|"+this._fixstr(efecto)+"|"+this._fixstr(pedimento)+"|"+this._fixstr(fechapedi)+"|"+this._fixstr(aduanaped)
       cbuff = cbuff+"|"+cline+"|"+CHR(13)+CHR(10)
    ENDSCAN
    USE IN (ccursor1)
    STRTOFILE(cbuff, cfilename)
    RETURN cfilename
   ENDFUNC
**
   HIDDEN FUNCTION _FixStr
    LPARAMETERS pcstring
    IF ISNULL(pcstring)
       RETURN ""
    ENDIF
    pcstring = ALLTRIM(CHRTRAN(pcstring, "|", ""))
    RETURN pcstring
   ENDFUNC
**
ENDDEFINE
**
FUNCTION CFDProbarOpenSSL
 LPARAMETERS pcopenssl
 cfdconf.ultimoerror = ""
 IF EMPTY(pcopenssl)
    pcopenssl = cfdconf.openssl
 ENDIF
 IF EMPTY(pcopenssl) .OR.  .NOT. FILE(ADDBS(pcopenssl)+"OPENSSL.EXE")
    cfdconf.ultimoerror = "No se encontro el archivo OPENSSL.EXE en la ruta indicada"
    RETURN .F.
 ENDIF
 pcopenssl = getshortname(ADDBS(FULLPATH(pcopenssl))+"OPENSSL.EXE")
 LOCAL cbatfile, ctempfile
 cbatfile = gettempfile("BAT")
 ctempfile = JUSTSTEM(cbatfile)
 LOCAL cbuff
 cbuff = pcopenssl+" version > {tempFile}.out"+CHR(13)+CHR(10)
 cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 LOCAL cresult, lok
 cresult = FILETOSTR(ctempfile+".OUT")
 ERASE (ctempfile+".*")
 lok = (ATC("OpenSSL", cresult)<>0)
 cfdconf.ultimoerror = cresult
 RETURN lok
ENDFUNC
**
PROCEDURE CFDITimbraFacturaxion
 PARAMETER cxmlcfd, ccodusuarioprov, ccodusuario, cidsucursal
ENDPROC
**
FUNCTION CFDCadenaCBB
 LPARAMETERS pcarchivoxml
 LOCAL ccodifica, crfcemisor, crfcreceptor, ctotal, cuuid
 ccodifica = ''
 ocfdi = CREATEOBJECT('MSXML2.DOMdocument')
 ocfdi.load(pcarchivoxml)
 IF (ocfdi.parseerror.errorcode<>0)
    myerr = oxml.parseerror
    cfdconf.ultimoerror = myerr.reason
    ccodifica = ''
 ELSE
    olnode = ocfdi.selectsinglenode("//cfdi:Comprobante/cfdi:Emisor")
    IF ISNULL(olnode)
       cfdconf.ultimoerror = "XML inválido. Nodo <cfdi:Emisor> no presente."
       ccodifica = ''
    ELSE
       crfcemisor = olnode.getattribute("Rfc")
    ENDIF
    olnode = ocfdi.selectsinglenode("//cfdi:Comprobante/cfdi:Receptor")
    IF ISNULL(olnode)
       cfdconf.ultimoerror = "XML inválido. Nodo <cfdi:Receptor> no presente."
       ccodifica = ''
    ELSE
       crfcreceptor = olnode.getattribute("Rfc")
       crfcreceptor = olnode.getattribute("UsoCFDI")
    ENDIF
    olnode = ocfdi.selectsinglenode("//cfdi:Comprobante")
    IF ISNULL(olnode)
       cfdconf.ultimoerror = "XML inválido. Nodo <cfdi:Comprobante> no presente."
       ccodifica = ''
    ELSE
       ctotal = olnode.getattribute("Total")
       ctotal = PADL(SUBSTR(ctotal, 1, ATC('.', ctotal)-1), 10, '0')+'.'+PADR(RIGHT(ctotal, LEN(ctotal)-ATC('.', ctotal)), 6, '0')
    ENDIF
    olnode = ocfdi.selectsinglenode("//cfdi:Comprobante/cfdi:Complemento/tfd:TimbreFiscalDigital")
    IF ISNULL(olnode)
       cfdconf.ultimoerror = "XML inválido. Nodo <tfd:TimbreFiscalDigital> no presente."
       ccodifica = ''
    ELSE
       cuuid = olnode.getattribute("UUID")
    ENDIF
 ENDIF
 ccodifica = '?re='+crfcemisor+'&rr='+crfcreceptor+'&tt='+ctotal+'&id='+cuuid
 RETURN ccodifica
ENDFUNC
**
FUNCTION CFDGoogleQR
 LPARAMETERS tcdato, tcdestino
 lcdimensiones = '200x200'
 IF VARTYPE(tcdestino)<>"C"
    lcimagen = gettempfile("PNG")
 ELSE
    lcimagen = tcdestino
 ENDIF
 DECLARE LONG URLDownloadToFile IN "urlmon" LONG, STRING, STRING, LONG, LONG
 lcurl ="https://chart.googleapis.com/chart?cht=qr&chs=" +  lcdimensiones + "&chld=M&chl=" + cfdgetescaped(tcdato, 0x2000) 
 nretval = urldownloadtofile(0, lcurl, lcimagen, 0, 0)
 IF nretval<>0
    cfdconf.ultimoerror = "ERROR "+TRANSFORM(nretval, "")
 ENDIF
 RETURN IIF(nretval==0, lcimagen, "")
ENDFUNC
**
FUNCTION CFDCreateQR
 LPARAMETERS tcdato, tcdestino
 IF VARTYPE(tcdestino)<>"C"
    lcimagen = gettempfile("PNG")
 ELSE
    lcimagen = tcdestino
 ENDIF
 generatefile(tcdato, lcimagen)
 RETURN lcimagen
ENDFUNC
**
FUNCTION CFDGetEscaped
 LPARAMETERS tcsource, tnflag
 DECLARE INTEGER UrlEscape IN shlwapi STRING, STRING @, INTEGER @, INTEGER
 LOCAL ctarget, nbufsize
 nbufsize = LEN(tcsource)*4
 ctarget = REPLICATE(CHR(0), nbufsize)
 = urlescape(tcsource, @ctarget, @nbufsize, tnflag)
 RETURN LEFT(ctarget, nbufsize)
ENDFUNC
**
FUNCTION CFDEVL
 LPARAMETERS puvalue, pudefault
 RETURN IIF(EMPTY(puvalue), pudefault, puvalue)
ENDFUNC
**
FUNCTION CFDBuffer
 LPARAMETERS pcitemlist, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19
 LOCAL obuff, i, cprop
 obuff = CREATEOBJECT("Custom")
 LOCAL cpropname, upropvalue, ncount
 LOCAL aprops[1]
 ncount = ALINES(aprops, STRTRAN(pcitemlist, ",", CHR(13)+CHR(10)))
 FOR i = 1 TO MIN(ncount, 20)
    cpropname = aprops(i)
    upropvalue = EVALUATE("P"+ALLTRIM(STR(i-1)))
    obuff.addproperty(cpropname, upropvalue)
 ENDFOR
 RETURN obuff
ENDFUNC
**
FUNCTION obtenVersion
 LPARAMETERS pnversion
 RETURN cfdversions.tostring(pnversion)
ENDFUNC
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
DEFINE CLASS CFDLineaReporteMensual AS Custom
 rfc = ""
 serie = ""
 folio = 0
 noaprobacion = ""
 anoaprobacion = ""
 fechahora = { :}
 total = 0.00 
 traslados = 0.00 
 estado = 0
 efecto = ""
 pedimento = ""
 fechapedimento = ""
 aduana = ""
ENDDEFINE
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
