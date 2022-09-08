* @ValidationCode : Mjo5MjU5MDcyNTU6Q3AxMjUyOjE2MzI3NTcxMDM5Mjk6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Sep 2021 21:38:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.DEBIT.CARD.REQ.CUS.VAL

    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_GTS.COMMON
    $INSERT  I_ENQUIRY.COMMON
            
    $INSERT I_F.MBL.CREDIT.CARD.DETAILS
    $INSERT I_F.MBL.H.CARD.REQ.LIST
    $INSERT I_F.MBL.DEBIT.CARD.REQ
    
    $USING EB.Reports
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.DatInterface
    $USING EB.ErrorProcessing
    $USING EB.Display
    
    $USING AC.AccountOpening
    $USING ST.CompanyCreation
    $USING ST.Customer
    
    $USING AA.Framework
    $USING AA.Account
    $USING AA.Customer


    !DEBUG

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN

INIT:
*==========
    !DEBUG

    FN.AC = 'F.ACCOUNT'
    F.AC = ''

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    
    
    FN.CRD.LIST='F.MBL.CREDIT.CARD.DETAILS'
    F.CRD.LIST=''

    FN.CRD.REQ = 'F.MBL.DEBIT.CARD.REQ'
    F.CRD.REQ = ''


    Y.FLAG = ''
    Y.MANDATORY.INFO = ''


RETURN



OPEN.FILES:
*===========
    EB.DataAccess.Opf(FN.CRD.LIST,F.CRD.LIST)
    EB.DataAccess.Opf(FN.CRD.REQ,F.CRD.REQ)
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.CUS,F.CUS)

RETURN


PROCESS:
*===========

    !DEBUG

    
    Y.ACC.NO = EB.SystemTables.getIdNew()
    Y.CUSTOMER.ID.GIVEN = EB.SystemTables.getComi()
    
    EB.DataAccess.FRead(FN.AC,Y.ACC.NO,R.AC,F.AC,Y.ERR.AC)
    Y.ARR.ID = R.AC<AC.AccountOpening.Account.ArrangementId>
    
        
    PROP.CLASS = 'CUSTOMER'
    PROPERTY = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    CUS.R.REC = RAISE(RETURN.VALUES)
    Y.CUSTOMER.ID.LIST = CUS.R.REC<AA.Customer.Customer.CusCustomer>
    
    Y.CUSTOMER.ID.LIST.CNT = DCOUNT ( Y.CUSTOMER.ID.LIST, @VM)
    
    IF Y.CUSTOMER.ID.LIST.CNT EQ 1 THEN
        
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER.ID.LIST,R.CUS.GIVEN,F.CUS,Y.ERR.CUS.GIVEN)
        Y.GIVEN.CUS.SECTOR = R.CUS.GIVEN<ST.Customer.Customer.EbCusSector>
        
        IF Y.GIVEN.CUS.SECTOR EQ '9999' THEN
                                
            Y.TEXT = 'INVALID CUSTOMER SECTOR CODE 9999'
            
            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
         
        END
        
        
        IF (Y.GIVEN.CUS.SECTOR EQ '1001') OR (Y.GIVEN.CUS.SECTOR EQ '1002') THEN
                      
            RETURN
            
        END
       
        ELSE
            
            GOSUB CORPORATE.SINGLE
            
        END
        
    END
    
     
    IF Y.CUSTOMER.ID.LIST.CNT NE 1 THEN
    
    
        Y.CUSTOMER.ID.GIVEN = EB.SystemTables.getComi()
    
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER.ID.GIVEN,R.CUS.GIVEN,F.CUS,Y.ERR.CUS.GIVEN)
        Y.GIVEN.CUS.SECTOR = R.CUS.GIVEN<ST.Customer.Customer.EbCusSector>
    
    
        IF Y.GIVEN.CUS.SECTOR EQ '9999' THEN
                                
            Y.TEXT = 'INVALID CUSTOMER SECTOR CODE 9999'
            
*            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
*            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
         
        END
    
        IF (Y.GIVEN.CUS.SECTOR EQ '1001') OR (Y.GIVEN.CUS.SECTOR EQ '1002') THEN

            GOSUB INDIVIDUAL
     
        END

        
        ELSE

            GOSUB CORPORATE

        END

        RETURN



INDIVIDUAL:
    
        !DEBUG
    
        FIND Y.CUSTOMER.ID.GIVEN IN Y.CUSTOMER.ID.LIST SETTING Y.FM.POS, Y.VM.POS THEN
    
            !LOCATE Y.CUSTOMER.ID.GIVEN IN Y.CUSTOMER.ID.LIST SETTING Y.CUS.POS THEN
    
            GOSUB MANDATORY.CHECK
        
        END
    
        ELSE
      
            Y.TEXT = 'INVALID CUSTOMER'
            
*            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
*            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
        
            RETURN
        
    
        END
 
        RETURN


CORPORATE.SINGLE:
    
        !DEBUG
    
        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.ID",Y.CUST.ID.CORP.POS)
    
        Y.CUST.ID.CORP.LIST =R.CUS.GIVEN<ST.Customer.Customer.EbCusLocalRef,Y.CUST.ID.CORP.POS>
    
*        LOCATE Y.CUSTOMER.ID.GIVEN IN Y.CUST.ID.CORP.LIST SETTING Y.CUS.POS THEN
*
*
*            Y.TEST = Y.CUS.POS
*        END
    
        FIND Y.CUSTOMER.ID.GIVEN IN Y.CUST.ID.CORP.LIST SETTING Y.FM.POS, Y.VM.POS,Y.SVM THEN
    
            GOSUB MANDATORY.CHECK
        
        END
    
        !IF Y.CUS.POS EQ '' THEN
        !IF Y.CUS.POS GT Y.CUSTOMER.ID.LIST.CNT THEN
        
        ELSE
            Y.TEXT = 'INVALID CUSTOMER'
            
*            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
*            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
        
            RETURN
        
    
        END


        RETURN

CORPORATE:
    
        !DEBUG
        
        FIND Y.CUSTOMER.ID.GIVEN IN Y.CUSTOMER.ID.LIST SETTING Y.FM.POS, Y.VM.POS THEN
    
            !LOCATE Y.CUSTOMER.ID.GIVEN IN Y.CUSTOMER.ID.LIST SETTING Y.CUS.POS THEN
    
            GOSUB MANDATORY.CHECK
        
        END
    
        ELSE
      
            Y.TEXT = 'INVALID CUSTOMER'
            
*            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
*            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
        
            RETURN
        
    
        END
 
        RETURN


MANDATORY.CHECK:
    
        !DEBUG
    
        !CALL F.READ(FN.CUS,Y.CUSTOMER.ID.GIVEN,R.CUS,F.CUS,Y.ERR.CUS)
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER.ID.GIVEN,R.CUS.GIVEN.MAN.CHK,F.CUS,Y.ERR.CUS.GIVEN)

        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.FNAME",Y.LT.CUS.FNAME.POS)
        Y.FATHER.NAME = R.CUS.GIVEN.MAN.CHK<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.FNAME.POS>
    

        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.MNAME",Y.LT.CUS.MNAME.POS)
        Y.MOTHER.NAME = R.CUS.GIVEN.MAN.CHK<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.MNAME.POS>
    
    

        Y.CUS.GENDER = R.CUS.GIVEN.MAN.CHK<ST.Customer.Customer.EbCusGender>
        Y.CUS.CELL = R.CUS.GIVEN.MAN.CHK<ST.Customer.Customer.EbCusSmsOne>
        Y.CUS.DOB = R.CUS.GIVEN.MAN.CHK<ST.Customer.Customer.EbCusDateOfBirth>
    
    
    
    
        IF (Y.CUS.GENDER EQ '') THEN

            Y.MAN.FLAG = 'YES'
            Y.MANDATORY.INFO = Y.MANDATORY.INFO:' GENDER'

        END

        IF (Y.CUS.CELL EQ '') THEN

            Y.MAN.FLAG = 'YES'
            Y.MANDATORY.INFO = Y.MANDATORY.INFO:' PHONE'

        END


        IF (Y.CUS.DOB EQ '') THEN

            Y.MAN.FLAG = 'YES'

            Y.MANDATORY.INFO = Y.MANDATORY.INFO:' DOB'

        END

        IF (Y.FATHER.NAME EQ '') THEN

            Y.MAN.FLAG = 'YES'

            Y.MANDATORY.INFO = Y.MANDATORY.INFO:' FATHER NAME'

        END

        IF (Y.MOTHER.NAME EQ '') THEN

            Y.MAN.FLAG = 'YES'
            Y.MANDATORY.INFO = Y.MANDATORY.INFO:' MOTHER NAME'

        END


        IF Y.MAN.FLAG EQ 'YES' THEN
       
            Y.TEXT = 'MANDATORY INFORMATION MISSING: ':Y.MANDATORY.INFO
            
*            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
*            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
        
            RETURN

        END
        
        RETURN
        
    END
    