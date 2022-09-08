SUBROUTINE GB.MBL.BA.SDSA.DUP.CHK
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    : TELLER VERSION
*Attached As    : BEFORE AUTH ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 14/06/2020 -                            Retrofit   - MD.KAMRUL HASAN
*                                                 FDS Pvt Ltd
* 28/06/2020 -                            - Sarowar Mortoza
** UNNECESSARY CODE REMOVE AND LOCAL REFERENCE FIELD READ
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING ST.CompanyCreation
    $USING FT.Contract
    $USING AC.AccountOpening
    $INSERT I_F.BD.SDSA.ENTRY.DETAILS
    $INSERT I_F.BD.SDSA.AC.REFNO
    $USING EB.DataAccess
    $USING EB.Updates
    $USING EB.SystemTables
    $USING  EB.ErrorProcessing

    IF EB.SystemTables.getVFunction() NE 'A' THEN RETURN


    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

INIT:

    FN.AC = 'F.ACCOUNT'
    FP.AC = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.SDSA.DET = 'F.BD.SDSA.ENTRY.DETAILS'
    F.SDSA.DET = ''

    FN.SDSA.REF = 'F.BD.SDSA.AC.REFNO'
    F.SDSA.REF = ''
    Y.REF.NO = ''
    Y.SDSA.ORG.CNT = ''
    Y.SDSA.ADJ.CNT = ''
    Y.TOT.ADJ.AMT = 0
    Y.TOT.ORG.AMT = 0
    Y.TOT.OUT.AMT = 0

    Y.APP.NAME ="FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.ORG.ADJ":VM:"LT.A.L"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.FT.ORGADJ.POS=FLD.POS<1,1>
    Y.FT.AL.POS=FLD.POS<1,2>
    
RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.AC,FP.AC)
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.SDSA.DET,F.SDSA.DET)
    EB.DataAccess.Opf(FN.SDSA.REF,F.SDSA.REF)

RETURN

PROCESS:

    BEGIN CASE
        CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'A'
            Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef)
            EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)

            Y.DR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef)
            EB.DataAccess.FRead(FN.AC,Y.DR.AC,AC.REC,FP.AC,AC.ERR)
            Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>

            IF R.SDSA.DET.REC THEN
                Y.SDSA.AC.NO = R.SDSA.DET.REC<BD.SDSA.AC.NUMBER>
                IF Y.DR.AC NE Y.SDSA.AC.NO THEN
                    EB.SystemTables.setEtext("Invalid Debit Account for the Existing Reference Number !!")
                    EB.ErrorProcessing.StoreEndError()
                END
            END

        CASE  EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'L'
            Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef)
            EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)

            Y.CR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
            EB.DataAccess.FRead(FN.AC,Y.CR.AC,AC.REC,FP.AC,AC.ERR)
            Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>

            IF R.SDSA.DET.REC THEN
                Y.SDSA.AC.NO = R.SDSA.DET.REC<BD.SDSA.AC.NUMBER>
                IF Y.CR.AC NE Y.SDSA.AC.NO THEN
                    EB.SystemTables.setEtext("Invalid Credit Account for the Existing Reference Number !!")
                    EB.ErrorProcessing.StoreEndError()
                END
            END

    END CASE

RETURN
END
