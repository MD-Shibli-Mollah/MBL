* @ValidationCode : MjoxMjc5NDk2OTc1OkNwMTI1MjoxNjMwNDI0MjY1NjUxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 31 Aug 2021 21:37:45
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.I.CHQ.PRINT
*-----------------------------------------------------------------------------
*Subroutine Description:
* This routine is use for Cheque Number, Cheque no not zero or blank validation
*Subroutine Type:
*Attached To    : version(CHEQUE.ISSUE,MBL.CDA.PRINT,CHEQUE.ISSUE,MBL.CA.PRINT,CHEQUE.ISSUE,MBL.CDB.PRINT,CHEQUE.ISSUE,MBL.SBA.PRINT,CHEQUE.ISSUE,MBL.SA.PRINT)
*Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1:
* R10 LEGACY ROUTINE NAME:V.MBL.CHQ.ISSUE
* 23/02/2020 -                             Retrofit   - MD. SAROWAR MORTOZA
*                                                 FDS Bangladesh Limited
* MODIFIED BY                              MD SHIBLI MOLLAH
* ON 07/02/2021                                   FDS Bangladesh Limited

*-----------------------------------------------------------------------------
* Modification History 2:
* USER CO CODE FOR BOTH BNK AND ISL
* 31/08/2021 -
* MODIFIED BY                                     MD SHIBLI MOLLAH
* ON 07/02/2021                                   FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.ChqIssue
    $USING ST.ChqSubmit
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.OverrideProcessing
    $USING EB.Foundation
    $USING EB.ErrorProcessing
    $USING ST.CompanyCreation
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
    F.CHEQUE.REGISTER = ''
    FN.CHEQUE.REGISTER = 'F.CHEQUE.REGISTER'

*  Y.TXN.ID = EB.SystemTables.getIdNew()
    Y.ACCOUNT.ID = ''
    Y.CUSTOMER.ID = ''

    Y.CHQ.NO.CHK = 5

    Y.NULL = ''

RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.CHEQUE.REGISTER,F.CHEQUE.REGISTER)
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
* To check the number of previously unused cheques...
* And to generate an override if more than 5 cheques are unused..
***--------------
   
    Y.MNEMONIC = FN.CHEQUE.REGISTER[2,3]
    
    Y.CHQ.ISS.ID = EB.SystemTables.getIdNew()
    Y.CHQ.REG.ID = FIELD(Y.CHQ.ISS.ID,'.',1,2)

    R.CHEQUE.REGISTER = ''
    Y.CHEQUE.REGISTER.ERR = ''
    EB.DataAccess.FRead(FN.CHEQUE.REGISTER,Y.CHQ.REG.ID,R.CHEQUE.REGISTER,F.CHEQUE.REGISTER,Y.CHEQUE.REGISTER.ERR)
    Y.CHQ.NO.HELD = R.CHEQUE.REGISTER<ST.ChqSubmit.ChequeRegister.ChequeRegNoHeld>
    
    IF Y.CHQ.NO.HELD GT Y.CHQ.NO.CHK THEN
        EB.SystemTables.setText("More than 5 Cheque Leaves not Presented")
        EB.OverrideProcessing.StoreOverride(CURR.NO)
    END

*---------------
**To input user branch code in company code
*---------------
    Y.CHQ.COM.CODE.POS=""
    Y.CHQ.NO.START.POS=""
    Y.APP.NAME ="CHEQUE.ISSUE"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.CHQ.COM.CODE":VM:"LT.CHQ.NO.START"
    FLD.POS = ""
    EB.Foundation.MapLocalFields(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.CHQ.COM.CODE.POS=FLD.POS<1,1>
    Y.CHQ.NO.START.POS=FLD.POS<1,2>

    IF Y.MNEMONIC EQ 'BNK' THEN

        Y.USER.CO.CODE = EB.SystemTables.getRUser()<5,1>
    END
    ELSE
        Y.USER.CO.CODE = EB.SystemTables.getRUser()<5,2>
    END
    
    Y.TEMP = EB.SystemTables.getRNew(ST.ChqIssue.ChequeIssue.ChequeIsLocalRef)
    Y.TEMP<1,Y.CHQ.COM.CODE.POS> = Y.USER.CO.CODE
    EB.SystemTables.setRNew(ST.ChqIssue.ChequeIssue.ChequeIsLocalRef, Y.TEMP)

*<-> TRACER
*    WriteData = Y.CHQ.ISS.ID:" ":Y.CHQ.REG.ID:" CHECK CO CODE ":Y.USER.CO.CODE:" ":Y.TEMP:" ":MYTEST
*    FileName = 'SHIBLI_CHEQUE.ISSUE.COCODE.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput
*<>
    
RETURN
*** </region>

END



