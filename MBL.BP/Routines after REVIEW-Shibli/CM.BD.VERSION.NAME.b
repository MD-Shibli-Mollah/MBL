* @ValidationCode : MjotMTI2MjE5NjY0NzpDcDEyNTI6MTYyNTY2NzQ3ODU5MTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 07 Jul 2021 20:17:58
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CM.BD.VERSION.NAME
*-----------------------------------------------------------------------------
* This routine is used as Input Routine to write version name
* in current Transaction
*-----------------------------------------------------------------------------
* Modification History : MD SHIBLI MOLLAH -- FDS --- 22ND DECEMBER 2020
*                      FINAL MODIFICATION --- FDS --- 2RD JUNE 2021
*-----------------------------------------------------------------------------
* Modification 2
* Extra INSERT removed
* EB.SystemTables.getApplication() line is outside CASE block
* assigned to Y.APP
* Modified by MD SHIBLI MOLLAH FDS -- on 07TH JUL 2021
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.VERSION
    $INSERT I_GTS.COMMON
    
    $USING LC.Contract
    $USING MD.Contract
    $USING AC.AccountOpening
    $USING TT.Contract
    $USING FT.Contract
    $USING AC.AccountClosure
    
    $USING ST.Customer
    $USING FT.AdhocChargeRequests
    $USING ST.ChqIssue
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING EB.SystemTables
*-----------------------------------------------------------------------------
    IF OFS.VAL.ONLY THEN RETURN
    GOSUB INITIALISE
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INITIALISE:
    FN.LC='F.LETTER.OF.CREDIT'
    F.LC=''
    FN.DR='F.DRAWINGS'
    F.DR=''
    FN.MD='F.MD.DEAL'
    F.MD=''
    FN.AC='F.ACCOUNT'
    F.AC=''
    FN.AC.CLS='F.ACCOUNT.CLOSURE'
    F.AC.CLS=''
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.TT = 'F.TELLER'
    F.TT = ''
*  -----INIT CUSTOMER, AC.CHARGE.REQUEST AND CHEQUE.ISSUE -------
    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    FN.AC.CHRG.REQ = 'F.AC.CHARGE.REQUEST'
    F.AC.CHRG.REQ = ''
    FN.CHQ.ISS = 'F.CHEQUE.ISSUE'
    F.CHQ.ISS = ''

    Y.VERSION.NAME=''
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.VER.NAME",Y.LC.VER.NAME)
    EB.LocalReferences.GetLocRef("DRAWINGS","LT.TF.VER.NAME",Y.DR.VER.NAME)
    EB.LocalReferences.GetLocRef("MD.DEAL","LT.TF.VER.NAME",Y.MD.VER.NAME)
    EB.LocalReferences.GetLocRef("ACCOUNT","LT.TF.VER.NAME",Y.AC.VER.NAME)
    EB.LocalReferences.GetLocRef("ACCOUNT.CLOSURE","LT.TF.VER.NAME",Y.ACCLS.VER.NAME)
    EB.LocalReferences.GetLocRef("FUNDS.TRANSFER","LT.TF.VER.NAME",Y.FT.VER.NAME)
    EB.LocalReferences.GetLocRef("TELLER","LT.TF.VER.NAME",Y.TT.VER.NAME)
    
    EB.LocalReferences.GetLocRef("CUSTOMER","LT.TF.VER.NAME",Y.CUS.VER.NAME)
    EB.LocalReferences.GetLocRef("AC.CHARGE.REQUEST","LT.TF.VER.NAME",Y.CHRG.VER.NAME)
    EB.LocalReferences.GetLocRef("CHEQUE.ISSUE","LT.TF.VER.NAME",Y.CHQ.VER.NAME)

RETURN
OPENFILES:
    EB.DataAccess.Opf(FN.LC,F.LC)
    EB.DataAccess.Opf(FN.DR,F.DR)
    EB.DataAccess.Opf(FN.MD,F.MD)
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.AC.CLS,F.AC.CLS)
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.TT,F.TT)
    
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.AC.CHRG.REQ,F.AC.CHRG.REQ)
    EB.DataAccess.Opf(FN.CHQ.ISS,F.CHQ.ISS)

RETURN
PROCESS:
    Y.APP = EB.SystemTables.getApplication()
    BEGIN CASE
        CASE Y.APP EQ 'LETTER.OF.CREDIT'
            Y.LC.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.LC,Y.LC.ID,R.LC.REC,F.LC,Y.LC.ERR)
            Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VER.NAME=EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)
            Y.VER.NAME<1,Y.LC.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(LC.Contract.LetterOfCredit.TfLcLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'DRAWINGS'
            Y.DR.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.DR,Y.DR.ID,R.DR.REC,F.DR,Y.DR.ERR)
            Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VER.NAME=EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrLocalRef)
            Y.VER.NAME<1,Y.DR.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(LC.Contract.Drawings.TfDrLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'MD.DEAL'
            Y.MD.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.MD,Y.MD.ID,R.MD.REC,F.MD,Y.MD.ERR)
            Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VER.NAME=EB.SystemTables.getRNew(MD.Contract.Deal.DeaLocalRef)
            Y.VER.NAME<1,Y.MD.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(MD.Contract.Deal.DeaLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'ACCOUNT'
            Y.AC.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.AC,Y.AC.ID,R.AC.REC,F.AC,Y.AC.ERR)
            Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VER.NAME=EB.SystemTables.getRNew(AC.AccountOpening.Account.LocalRef)
            Y.VER.NAME<1,Y.AC.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(AC.AccountOpening.Account.LocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'ACCOUNT.CLOSURE'
            Y.ACCLS.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.AC.CLS,Y.ACCLS.ID,R.ACCLS.REC,F.AC.CLS,Y.ACCLS.ERR)
            Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VER.NAME=EB.SystemTables.getRNew(AC.AccountClosure.AccountClosure.AclLocalRef)
            Y.VER.NAME<1,Y.ACCLS.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(AC.AccountClosure.AccountClosure.AclLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'FUNDS.TRANSFER'
            Y.FT.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.FT,Y.FT.ID,R.FT.REC,F.FT,Y.FT.ERR)
*--------ONLY VERSION IS TAKEN------------
* Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
            Y.VERSION.NAME = 'FUNDS.TRANSFER':Y.VERSION.NAME.1
            Y.VER.NAME=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
            Y.VER.NAME<1,Y.FT.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'TELLER'
            Y.TT.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.TT,Y.TT.ID,R.TT.REC,F.TT,Y.TT.ERR)
*   Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
            Y.VERSION.NAME = 'TELLER':Y.VERSION.NAME.1
            Y.VER.NAME=EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
            Y.VER.NAME<1,Y.TT.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef,Y.VER.NAME)
            
        CASE Y.APP EQ 'CUSTOMER'
            Y.CUS.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.CUS,Y.CUS.ID,R.CUS.REC,F.CUS,Y.CUS.ERR)
*  Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
            Y.VERSION.NAME = 'CUSTOMER':Y.VERSION.NAME.1
            Y.VER.NAME=EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)
            Y.VER.NAME<1,Y.CUS.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'AC.CHARGE.REQUEST'
            Y.CHRG.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.AC.CHRG.REQ,Y.CHRG.ID,R.CHRG.REC,F.AC.CHRG.REQ,Y.CHG.ERR)
*  Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
            Y.VERSION.NAME = 'AC.CHARGE.REQUEST':Y.VERSION.NAME.1
            Y.VER.NAME=EB.SystemTables.getRNew(FT.AdhocChargeRequests.AcChargeRequest.ChgLocalRef)
            Y.VER.NAME<1,Y.CHRG.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(FT.AdhocChargeRequests.AcChargeRequest.ChgLocalRef,Y.VER.NAME)
        CASE Y.APP EQ 'CHEQUE.ISSUE'
            Y.CHQ.ID=EB.SystemTables.getIdNew()
            EB.DataAccess.FRead(FN.CHQ.ISS,Y.CHQ.ID,R.CHQ.ISS,F.CHQ.ISS,Y.CHQ.ISS.ERR)
*   Y.VERSION.NAME=FIELD(EB.SystemTables.getPgmVersion(),',',2)
            Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
            Y.VERSION.NAME = 'CHEQUE.ISSUE':Y.VERSION.NAME.1
            Y.VER.NAME=EB.SystemTables.getRNew(ST.ChqIssue.ChequeIssue.ChequeIsLocalRef)
            Y.VER.NAME<1,Y.CHQ.VER.NAME> = Y.VERSION.NAME
            EB.SystemTables.setRNew(ST.ChqIssue.ChequeIssue.ChequeIsLocalRef,Y.VER.NAME)
            
    END CASE
    
*    WriteData = "VERSION NAME: ":Y.VERSION.NAME
*    FileName = 'SH_VER_21.txt'
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
    

**********************************************

RETURN
END