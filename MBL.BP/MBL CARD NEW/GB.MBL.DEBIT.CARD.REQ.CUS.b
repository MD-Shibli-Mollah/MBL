* @ValidationCode : Mjo3ODc2OTg0MTU6Q3AxMjUyOjE2MzI3NTYyNjkwODQ6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Sep 2021 21:24:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.DEBIT.CARD.REQ.CUS

    !PROGRAM GB.MBL.DEBIT.CARD.REQ.CUS

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


RETURN


OPEN.FILES:
*===========
    EB.DataAccess.Opf(FN.CRD.LIST,F.CRD.LIST)
    EB.DataAccess.Opf(FN.CRD.REQ,F.CRD.REQ)
  
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.CUS,F.CUS)

RETURN

PROCESS:

    !DEBUG
    
* EB.LocalReferences.GetLocRef("MBL.H.DEBIT.CARD.REQ","CARD.CUSTOMER",Y.CARD.CUSTOMER.POS)

 
    Y.ACC.NO = EB.SystemTables.getIdNew()

    EB.DataAccess.FRead(FN.AC,Y.ACC.NO,R.AC,F.AC,Y.ERR.AC)
    Y.ARR.ID = R.AC<AC.AccountOpening.Account.ArrangementId>
    
        
    PROP.CLASS = 'CUSTOMER'
    PROPERTY = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    CUS.R.REC = RAISE(RETURN.VALUES)
    Y.CUSTOMER.ID = CUS.R.REC<AA.Customer.Customer.CusCustomer>
    
    Y.CUSTOMER.ID.CNT = DCOUNT (Y.CUSTOMER.ID, @VM)
    
    IF Y.CUSTOMER.ID.CNT EQ 1 THEN
        
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER.ID,R.CUS,F.CUS,Y.ERR.CUS)
        Y.CUS.SECTOR = R.CUS<ST.Customer.Customer.EbCusSector>
        
        IF Y.CUS.SECTOR EQ '9999' THEN
                                
            Y.TEXT = 'INVALID CUSTOMER SECTOR CODE 9999'
            
            EB.SystemTables.setAf(DR.CARD.LOCAL.REF)
            EB.SystemTables.setAv(Y.CARD.CUSTOMER.POS)
            EB.SystemTables.setEtext(Y.TEXT)
            EB.ErrorProcessing.StoreEndError()
         
        END
        
        IF (Y.CUS.SECTOR EQ '1001') OR (Y.CUS.SECTOR EQ '1002') THEN

            GOSUB INDIVIDUAL
     
        END
        
        ELSE

            GOSUB CORPORATE

        END
        
    
    END

RETURN

INDIVIDUAL:

    !DEBUG
        
*    EB.LocalReferences.GetLocRef("MBL.H.DEBIT.CARD.REQ","CARD.CUSTOMER",Y.CARD.CUSTOMER.POS)
*
*
*    Y.TEMP = EB.SystemTables.getRNew(9)
*    Y.TEMP<1,Y.CARD.CUSTOMER.POS> = Y.CUSTOMER.ID
*    EB.SystemTables.setRNew(9,Y.TEMP)
*
*    tmpTLocref=EB.SystemTables.getTLocref();
*    tmpTLocref<Y.CARD.CUSTOMER.POS,7>="NOINPUT";
*    EB.SystemTables.setTLocref(tmpTLocref);

   
* Y.CUS = EB.SystemTables.getRNew(DEBIT.CARD.CARD.CUSTOMER)
    EB.SystemTables.setRNew(DEBIT.CARD.CARD.CUSTOMER, Y.CUSTOMER.ID)

RETURN
                
CORPORATE:

    !DEBUG
    
    EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.CRP.MDES",Y.LT.CUS.CRP.MDES.POS)

    Y.CORP.MGMT.DESGN = R.CUS<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.CRP.MDES.POS>
       

    IF Y.CORP.MGMT.DESGN EQ 'SOLE PROPRIETOR' THEN

        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.ID",Y.CUST.ID.CORP.POS)

        Y.CUST.ID.CORP =R.CUS<ST.Customer.Customer.EbCusLocalRef,Y.CUST.ID.CORP.POS>
            
*        EB.LocalReferences.GetLocRef("MBL.H.DEBIT.CARD.REQ","CARD.CUSTOMER",Y.CARD.CUSTOMER.POS)
*
*        Y.TEMP = EB.SystemTables.getRNew(9)
*        Y.TEMP<1,Y.CARD.CUSTOMER.POS> = Y.CUST.ID.CORP
*        EB.SystemTables.setRNew(9,Y.TEMP)
*
*        tmpTLocref=EB.SystemTables.getTLocref();
*        tmpTLocref<Y.CARD.CUSTOMER.POS,7>="NOINPUT";
*        EB.SystemTables.setTLocref(tmpTLocref);
*  Y.CUS = EB.SystemTables.getRNew(DEBIT.CARD.CARD.CUSTOMER)
        EB.SystemTables.setRNew(DEBIT.CARD.CARD.CUSTOMER,Y.CUSTOMER.ID)
        RETURN
    END
RETURN
END