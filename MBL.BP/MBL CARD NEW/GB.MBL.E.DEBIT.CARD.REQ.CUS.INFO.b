SUBROUTINE GB.MBL.E.DEBIT.CARD.REQ.CUS.INFO(Y.RETURN)

    !   PROGRAM GB.MBL.E.DEBIT.CARD.REQ.CUS.INFO
    
        
    
*NOFILE.GB.MBL.E.DEBIT.CARD.REQ.CUS.INFO
*
*ENQ MBL.ENQ.GB.MBL.E.DEBIT.CARD.REQ.CUS.INFO


    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_GTS.COMMON
    $INSERT  I_ENQUIRY.COMMON
            
    $INSERT I_F.MBL.CREDIT.CARD.DETAILS
    $INSERT I_F.MBL.H.CARD.REQ.LIST
    
    !$INSERT I_F.MBL.H.DEBIT.CARD.REQ
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



    GOSUB INIT
    GOSUB PROCESS
    !  GOSUB WRITE.DETAILS

RETURN
******
INIT:
******

    !DEBUG
   
    FN.AC = 'F.ACCOUNT'
    F.AC = ''

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    
    
    FN.CRD.LIST='F.MBL.CREDIT.CARD.DETAILS'
    F.CRD.LIST=''

    FN.CRD.REQ = 'F.MBL.DEBIT.CARD.REQ'
    F.CRD.REQ = ''
    
    FN.CRD.LIST= 'F.MBL.H.CARD.REQ.LIST'
    F.CRD.LIST = ''
    
    
    EB.DataAccess.Opf(FN.CRD.LIST,F.CRD.LIST)
    EB.DataAccess.Opf(FN.CRD.REQ,F.CRD.REQ)
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.CUS,F.CUS)

    Y.NAME = ''
    Y.GENDER =''
    Y.AC.ID = ''
    Y.CUS.STREET = ''
    Y.TOWN = ''
    Y.CELL = ''
    Y.DOB = ''
    Y.FATHER.NAME = ''
    Y.MOTHER.NAME = ''
    Y.AC.TYPE = ''

    
    LOCATE "Y.DATE" IN EB.Reports.getEnqSelection()<2,1> SETTING POS THEN
        Y.SEL.DATE=EB.Reports.getEnqSelection()<4,POS>
    END
    

    !   Y.SEL.DATE = '20150805'

RETURN

*********
PROCESS:
*********

    !DEBUG

    EB.DataAccess.FRead(FN.CRD.LIST,Y.SEL.DATE,R.CRD.LIST,F.CRD.LIST,Y.ERR.CRD.LIST)
    Y.AC = R.CRD.LIST<CARD.LIST.ACCOUNT.NUMBER>
    Y.AC.CNT= DCOUNT(Y.AC,VM)

    FOR I = 1 TO Y.AC.CNT

        Y.AC.ID = R.CRD.LIST<CARD.LIST.ACCOUNT.NUMBER,I>

        IF Y.AC.ID[2,3] NE '111' THEN
            Y.AC.TYPE = '13'
        END ELSE
            Y.AC.TYPE = '15'
        END


        EB.DataAccess.FRead(FN.CRD.REQ,Y.AC.ID,R.AC,F.CRD.REQ,Y.ERR.AC)
        
        !EB.LocalReferences.GetLocRef("MBL.H.DEBIT.CARD.REQ","CARD.CUSTOMER",Y.CARD.CUSTOMER.POS)
        !Y.CUS.ID = R.AC<9,Y.CARD.CUSTOMER.POS>
        
        Y.CUS.ID =R.AC<DEBIT.CARD.CARD.CUSTOMER>
        
        
        
        EB.DataAccess.FRead(FN.CUS,Y.CUS.ID,R.CUS,F.CUS,Y.ERR.CUS)
     

        Y.NAME = R.CUS<ST.Customer.Customer.EbCusNameOne>
        
        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.FNAME",Y.LT.CUS.FNAME.POS)
        Y.FATHER.NAME = R.CUS<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.FNAME.POS>
    

        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.MNAME",Y.LT.CUS.MNAME.POS)
        Y.MOTHER.NAME = R.CUS<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.MNAME.POS>
    
        Y.GENDER = R.CUS<ST.Customer.Customer.EbCusGender>
        Y.CELL = R.CUS<ST.Customer.Customer.EbCusSmsOne>
        Y.DOB = R.CUS<ST.Customer.Customer.EbCusDateOfBirth>
        
        !Y.CUS.STREET = R.CUS<ST.Customer.Customer.EbCusStreet>
        Y.CUS.STREET = R.CUS<ST.Customer.Customer.EbCusAddress>
        
        !Y.TOWN = R.CUS<ST.Customer.Customer.EbCusTownCountry>
        EB.LocalReferences.GetLocRef("CUSTOMER","LT.CUS.PR.PS",Y.LT.CUS.PR.PS.POS)
        Y.TOWN =R.CUS<ST.Customer.Customer.EbCusLocalRef,Y.LT.CUS.PR.PS.POS>
        
        Y.LEGAL.DOC.NAME = R.CUS<ST.Customer.Customer.EbCusLegalDocName>
        
        
****************************IF USE ONLY NID ******************************************

*        FIND 'NATIONAL.ID' IN Y.LEGAL.DOC.NAME SETTING Y.POS1,Y.POS2 THEN
*
*            Y.LEGAL.DOC.NAME = R.CUS<ST.Customer.Customer.EbCusLegalDocName,Y.POS2>
*            Y.LEGAL.ID= R.CUS<ST.Customer.Customer.EbCusLegalId,Y.POS2>
*
*        END

**************************************************************************************
    
        Y.LEGAL.ID = R.CUS<ST.Customer.Customer.EbCusLegalId>
        Y.MAIL = R.CUS<ST.Customer.Customer.EbCusEmailOne>


        GOSUB RESULT.DATA

    NEXT I

RETURN

*************
RESULT.DATA:
*************

    !DEBUG

    !Y.RETURN<-1>=Y.AC.ID:"*":Y.NAME:"*":Y.CUS.ID:"*":Y.CELL:"*":Y.CUS.STREET:"*":Y.TOWN:"*":Y.LEGAL.DOC.NAME:"*":Y.LEGAL.ID:"*":Y.GENDER:"*":Y.DOB:"*":Y.MAIL
    Y.RETURN<-1>=Y.NAME:"*":Y.GENDER:"*":Y.AC.ID:"*":Y.AC.TYPE:"*":Y.CUS.STREET:"*":Y.TOWN:"*":Y.CELL:"*":Y.DOB:"*":Y.FATHER.NAME:"*":Y.MOTHER.NAME

*****                1          2            3          4               5            6            7          8            9                 10

RETURN


!WRITE.DETAILS:
*********************************
!   DEBUG
!  Y.DIR='./sajib.bp'
! Y.FILENAME= "CUS.INFO":".csv"

!  OPENSEQ Y.DIR,Y.FILENAME TO C.OUT ELSE
!     CREATE C.OUT ELSE
!        CRT "UNABLE TO CREATE FILE"
!   END
!  END
!  LINE.FEED = CHARX(10)
!  CONVERT FM TO LINE.FEED IN Y.RETURN
!   WRITESEQ Y.RETURN TO C.OUT ELSE
!      CRT "UNABLE TO WRITE"
!       CLOSESEQ C.OUT
!  END

!   RETURN


RETURN
END
