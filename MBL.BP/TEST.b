* @ValidationCode : MjotNTIwMzIzNTc6Q3AxMjUyOjE1OTc2NTEyNTc5NjE6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Aug 2020 14:00:57
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0


* @AUTHOR         : MD SHIBLI MOLLAH

*THIS CONVERSION ROUTINE TAKES DATE TIME THEN CONVERT THE DATE ONLY TO REGULAR DATE FORMET******

*SUBROUTINE TEST
PROGRAM TEST
    
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
* $USING EB.Reports
    $USING ST.CompanyCreation

    GOSUB PROCESS
RETURN

*--------------------------------------------------------------------------------------------------------------------------
PROCESS:
*--------------------------------------------------------------------------------------------------------------------------
    ST.CompanyCreation.LoadCompany('BNK')
*    Y.DATE='20190101 20200202'
*    Y.DATE.1=''
*    Y.F.DATE.2=''
**  Y.DATE=EB.Reports.getOData()
*    DEBUG
*    Y.DATE.LEN = LEN(Y.DATE)
*    PRINT Y.DATE.LEN
*    DEBUG
*    IF Y.DATE.LEN GT 8 THEN
*
*        Y.F.DATE = Y.DATE[1,8]
*        Y.F.DATE.1=ICONV(Y.F.DATE,'D4')
*        Y.F.DATE.2=OCONV(Y.F.DATE.1,'D')
****************************************************************************
*        Y.L.DATE = Y.DATE[10,17]
*        Y.L.DATE.1=ICONV(Y.L.DATE,'D4')
*        Y.L.DATE.2=OCONV(Y.L.DATE.1,'D')
*        BOOK.DATE = Y.F.DATE.2:' to ':Y.L.DATE.2
*        CRT BOOK.DATE
**  EB.Reports.setOData(Y.F.DATE.2:' ':Y.L.DATE.2)
*        DEBUG
*  END
  
  
    Y.ID.NEW = 'TF1916406653;99'
    DEBUG
    Y.ID.LEN = LEN(Y.ID.NEW)
    Y.ID.INITLEN = Y.ID.NEW[1,2]
    DEBUG
    
*****************************************
*    IF Y.ID.LEN LT 12 AND Y.ID.INITLEN NE 'TF' THEN
*        EB.SystemTables.setEtext("Please provide a valid LC ID")
*        EB.ErrorProcessing.StoreEndError()
*    END
*****************************************************************************
    Y.ID.NEW.COLON = Y.ID.NEW[13,1]
    Y.ID.NEW.1 = Y.ID.NEW[1,12]
    Y.ID.NEW.2 = Y.ID.NEW[14,2]
    Y.ID.FIN = Y.ID.NEW.1:'-':Y.ID.NEW.2
    Y.CAT.ID = '2345 5678'
    Y.CAT.ID.LEN = LEN(Y.CAT.ID)
    
    IF Y.CAT.ID.LEN GT 4 THEN
        Y.CAT.ID.1 = Y.CAT.ID[1,4]
        Y.CAT.ID.2 = Y.CAT.ID[6,9]
    END
    PRINT Y.CAT.ID.1
    PRINT Y.CAT.ID.2
RETURN
END