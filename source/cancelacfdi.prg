**
DEFINE CLASS VFP_WebService AS Custom
 serror = ""
 istatus = 0
 surl_ws = ""
**
   FUNCTION GetStamp
    LPARAMETERS us, pas, uuid, rfc, cerpem, keyenc
    sxmlrequest = this.createrequest(us, pas, uuid, rfc, cerpem, keyenc)
    pxmlresponse = "TIMBRECAN"+'.xml'
    this.istatus = this.ejecutaws(this.surl_ws, sxmlrequest, pxmlresponse)
    IF this.istatus<>0
       RETURN ""
    ENDIF
    sxmlresponse = FILETOSTR(pxmlresponse)
    RETURN sxmlresponse
   ENDFUNC
**
   FUNCTION EjecutaWS
    LPARAMETERS purl_wsdl, pfilerequest, pfileresponse
    TRY
       STRTOFILE(pfilerequest, 'request.txt', 0)
       ohttp = CREATEOBJECT('Msxml2.ServerXMLHTTP.6.0')
       ohttp.open("POST", purl_wsdl, .F.)
       ohttp.setrequestheader("User-Agent", "EjecutandoWS desde VFP - PortalFOX")
       ohttp.setrequestheader("Content-Type", "text/xml;charset=utf-8")
       ohttp.send(pfilerequest)
    CATCH TO loerr
       this.serror = "Error: "+TRANSFORM(loerr.errorno)+" Mensaje: "+loerr.message
       this.istatus = -1
    ENDTRY
    IF this.istatus<>0
       RETURN -1
    ENDIF
    IF ohttp.status=200
       respuestaws = ohttp.responsetext
       STRTOFILE(STRCONV(respuestaws, 9), pxmlresponse)
       this.istatus = 0
       this.serror = ""
       RETURN 0
    ELSE
       respuestaws = ohttp.responsetext
       this.serror = "Error: No se logró la conexión con el Web Service."
       this.istatus = -1
       RETURN -1
    ENDIF
   ENDFUNC
**
   FUNCTION CreateRequest
    LPARAMETERS us, pas, uuid, rfc, cerpem, keyenc
    TEXT TO sxmlrequest TEXTMERGE NOSHOW
           <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:can="http://facturacion.finkok.com/cancel" xmlns:apps="apps.services.soap.core.views" xmlns:can1="http://facturacion.finkok.com/cancellation">
           <soapenv:Header/>
             <soapenv:Body>
               <can:cancel>
                    <can:UUIDS>
                     <apps:uuids>
                        <can1:string><<uuid>></can1:string>
                     </apps:uuids>
                     </can:UUIDS>
                     <can:username><<us>></can:username>
                     <can:password><<pas>></can:password>
                     <can:taxpayer_id><<rfc>></can:taxpayer_id> 
                     <can:cer><<cerpem>></can:cer>
                     <can:key><<keyenc>></can:key>
                       <can:store_pending><<'T'>></can:store_pending>
               </can:cancel>
             </soapenv:Body>
           </soapenv:Envelope>
    ENDTEXT
    RETURN sxmlrequest
   ENDFUNC
**
   PROCEDURE BorraArchivo
    LPARAMETERS pfile
    IF FILE(pfile)
    ENDIF
   ENDPROC
**
   PROCEDURE Init
    LPARAMETERS tcurlws
    this.surl_ws = tcurlws
    this.istatus = 0
    this.serror = ""
   ENDPROC
**
ENDDEFINE
**
PROCEDURE CFDValidarKeyCer2
 LPARAMETERS pcarchivokey, pcpassword, pcopenssl
 pcopenssl = '\ssl\openSSL.exe'
 pcpassword = '12345678a'
 pcpassword2 = '@Vla-Draculero-1492'
 pcarchivokey = 'aad990814bp7_1210261233s.key'
 LOCAL cbatfile, ctempfile
 cbatfile = "archivo.BAT"
 ctempfile = "aad990814bp7_1210261233s.key"
 LOCAL cbuff
 cbuff = pcopenssl+" pkcs8 -inform DER -in {keyFile} -passin pass:"+CHR(34)+"{password}"+CHR(34)+" -out {tempFile}.pem"+crlf+pcopenssl+" rsa -in {tempFile}.pem -des3 -out aad990814bp7_1210261233s.key.enc -passout pass:"+CHR(34)+"{password2}"+CHR(34)+crlf+pcopenssl+" x509 -inform DER -outform PEM -in aad990814bp7_1210261233s.cer -pubkey -out aad990814bp7_1210261233s.cer.pem"
 cbuff = STRTRAN(cbuff, "{keyFile}", 'aad990814bp7_1210261233s.key')
 cbuff = STRTRAN(cbuff, "{password}", pcpassword)
 cbuff = STRTRAN(cbuff, "{password2}", pcpassword2)
 cbuff = STRTRAN(cbuff, "{tempFile}", ctempfile)
 STRTOFILE(cbuff, cbatfile)
 LOCAL owsh
 owsh = CREATEOBJECT("WScript.Shell")
 owsh.run(cbatfile, 0, .T.)
 RETURN
ENDPROC
**
