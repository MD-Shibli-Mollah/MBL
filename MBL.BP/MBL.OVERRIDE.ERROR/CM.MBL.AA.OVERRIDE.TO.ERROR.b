SUBROUTINE CM.MBL.AA.OVERRIDE.TO.ERROR
*-----------------------------------------------------------------------------
*Subroutine Description: this routine create for make limit 2(EXCESS.ID & NO.LINE) ovrride message into error message.
*Subroutine Type:
*Attached To    : FT version control & Lending Activity.api(FDBP,IDBP,PC & MBL.ACCT.TITLE.API).
* VERSION : DRAWINGS,MBL.IMPSP, DRAWINGS,MBL.IMPMAT, DRAWINGS,MBL.BTBSP, DRAWINGS,MBL.BTBMAT,
*           FUNDS.TRANSFER,MBL.AFTR.FTHP, FUNDS.TRANSFER,MBL.AA.ACDI
*Attached As    :
*-----------------------------------------------------------------------------
* Modification History :
* 08/12/2020 -                            Create   - MAHMUDUR RAHMAN UDOY,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_SOA.COMMON
    $INSERT I_GTS.COMMON
    
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING EB.LocalReferences
    $USING AA.Framework
    $USING AC.AccountOpening
    $USING AA.Account
    $USING AA.Limit
    $USING LC.Contract
      
    
    $USING FT.Contract
*-----------------------------------------------------------------------------
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
****************FOR CATCH OFS OVERRIDE ERROR Start*************
    Y.FLD.OVERRIDE = ''
    Y.OVERRIDE.COUNT = 0
    Y.FLD.OVERRIDE.ID = ''
    Y.FLD.OVERRIDE = EB.SystemTables.getRNew(V - 9) ;*FOR GET THIS OVERRIDE NEED OVERRIDE FIELD ADDED IN VERSION LABLE.
    Y.OVERRIDE.COUNT = DCOUNT(Y.FLD.OVERRIDE, '}')
    FOR U = 1 TO Y.OVERRIDE.COUNT
        Y.FLD.OVERRIDE.ID = FIELD(Y.FLD.OVERRIDE, '}',U)
        IF Y.FLD.OVERRIDE.ID EQ 'EXCESS.ID' THEN
            EB.SystemTables.setEtext('You have Excess over Limit')
            EB.ErrorProcessing.StoreEndError()
        END
    NEXT U
****************FOR CATCH OFS OVERRIDE ERROR End**************
    
    !**** For FT & DR version if debit account is a loan then this routine shows excess limit override message to error start****
    Y.DR.ACCT = ''
    Y.APP = EB.SystemTables.getApplication()
    IF Y.APP EQ 'DRAWINGS' THEN
        Y.DR.ACCT = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDrawdownAccount)
        IF Y.DR.ACCT EQ '' THEN RETURN
        AA.Account.GetActArrId(Y.DR.ACCT, Flag, RAccount, ReturnError)
        Y.ARR.ID = RAccount<AC.AccountOpening.Account.ArrangementId>
        Y.ARR.REC = AA.Framework.Arrangement.Read(Y.ARR.ID, Error)
        Y.PROP = Y.ARR.REC<AA.Framework.Arrangement.ArrProperty>
        Y.COUNT = DCOUNT(Y.PROP, @SM)
        FOR I = 1 TO Y.COUNT
            Y.PROP.NAME  = Y.PROP<1,1,I>
            IF Y.PROP.NAME EQ 'DRINTEREST' OR Y.PROP.NAME EQ 'PRINCIPALINT' THEN
                GOSUB ERROR.PROCESS
            END
        NEXT I
    END
    IF Y.APP EQ 'FUNDS.TRANSFER' THEN
        Y.DR.ACCT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        IF Y.DR.ACCT EQ '' THEN RETURN
        AA.Account.GetActArrId(Y.DR.ACCT, Flag, RAccount, ReturnError)
        Y.ARR.ID = RAccount<AC.AccountOpening.Account.ArrangementId>
        Y.ARR.REC = AA.Framework.Arrangement.Read(Y.ARR.ID, Error)
        Y.PROP = Y.ARR.REC<AA.Framework.Arrangement.ArrProperty>
        Y.COUNT = DCOUNT(Y.PROP, @SM)
        FOR I = 1 TO Y.COUNT
            Y.PROP.NAME  = Y.PROP<1,1,I>
            IF Y.PROP.NAME EQ 'DRINTEREST' OR Y.PROP.NAME EQ 'PRINCIPALINT' THEN
                GOSUB ERROR.PROCESS
            END
        NEXT I
    END
    !**** For FT & DR version if debit account is a loan then this routine shows excess limit override message to error end****
    !************FOR ARRAGEMETN LIMIT OVERRITE ERROR START**************
    Y.ARRANGEMENT.ID = ''
    Y.ARRANGEMENT.ID = c_aalocArrId
    IF Y.ARRANGEMENT.ID NE '' THEN
        Y.VM.COUNT = DCOUNT(OFS$OVERRIDES, @VM)
        IF Y.VM.COUNT THEN
            FOR I = 1 TO Y.VM.COUNT
                IF OFS$OVERRIDES<1,I> EQ "NO LINE ALLOCATED" THEN
                    EB.SystemTables.setAf(AA.Limit.Limit.LimLimitReference)
                    EB.SystemTables.setEtext("NO LINE ALLOCATED")
                    EB.ErrorProcessing.StoreEndError()
                END
                Y.FMT.4 = FIELD(OFS$OVERRIDES<1,I>," ",4)
                Y.FMT.9 = FIELD(OFS$OVERRIDES<1,I>," ",9)
                IF Y.FMT.4 EQ "Excess" AND Y.FMT.9 EQ "Limit" THEN
                    EB.SystemTables.setAf(AA.Limit.Limit.LimLimitReference)
                    EB.SystemTables.setEtext('You have Excess over Limit')
                    EB.ErrorProcessing.StoreEndError()
                END
            NEXT I
        END
        Y.SOA.OVERRIDES.COUNT = DCOUNT(SOA$OVERRIDES,'"')
        IF Y.SOA.OVERRIDES.COUNT THEN
            FOR I = 2 TO Y.SOA.OVERRIDES.COUNT STEP 2
                Y.OVERRIDE.ID = FIELD(SOA$OVERRIDES,'"',I)
                IF Y.OVERRIDE.ID EQ 'NO.LINE' THEN
                    EB.SystemTables.setAf(AA.Limit.Limit.LimLimitReference)
                    EB.SystemTables.setEtext('NO LINE ALLOCATED')
                    EB.ErrorProcessing.StoreEndError()
                END
                IF Y.OVERRIDE.ID EQ 'EXCESS.ID' THEN
                    EB.SystemTables.setAf(AA.Limit.Limit.LimLimitReference)
                    EB.SystemTables.setEtext('You have Excess over Limit')
                    EB.ErrorProcessing.StoreEndError()
                END
            NEXT I
        END
    END
    !************FOR ARRAGEMETN LIMIT OVERRITE ERROR END**************
   

RETURN

ERROR.PROCESS:
    Y.OVERRIDE.ID.LIST = EB.OverrideProcessing.getOveOverrideId()
    Y.COUNT = DCOUNT(Y.OVERRIDE.ID.LIST,VM)
    FOR I=1 TO Y.COUNT
        IF Y.OVERRIDE.ID.LIST<1,I> = 'EXCESS.ID' THEN
            EB.SystemTables.setEtext('You have Excess over Limit')
            EB.ErrorProcessing.StoreEndError()
        END
        IF Y.OVERRIDE.ID.LIST<1,I> = 'NO.LINE' THEN
            EB.SystemTables.setEtext('NO LINE ALLOCATED')
            EB.ErrorProcessing.StoreEndError()
        END
    NEXT I

    Y.VM.COUNT = DCOUNT(OFS$OVERRIDES, @VM)
    IF Y.VM.COUNT THEN
        FOR I = 1 TO Y.VM.COUNT
            IF OFS$OVERRIDES<1,I> EQ "NO LINE ALLOCATED" THEN
                EB.SystemTables.setEtext("NO LINE ALLOCATED")
                EB.ErrorProcessing.StoreEndError()
            END
            Y.FMT.4 = FIELD(OFS$OVERRIDES<1,I>," ",4)
            Y.FMT.9 = FIELD(OFS$OVERRIDES<1,I>," ",9)
            IF Y.FMT.4 EQ "Excess" AND Y.FMT.9 EQ "Limit" THEN
                EB.SystemTables.setEtext('You have Excess over Limit')
                EB.ErrorProcessing.StoreEndError()
            END
        NEXT I
    END
    Y.SOA.OVERRIDES.COUNT = DCOUNT(SOA$OVERRIDES,'"')
    IF Y.SOA.OVERRIDES.COUNT THEN
        FOR I = 2 TO Y.SOA.OVERRIDES.COUNT STEP 2
            Y.OVERRIDE.ID = FIELD(SOA$OVERRIDES,'"',I)
            IF Y.OVERRIDE.ID EQ 'NO.LINE' THEN
                EB.SystemTables.setEtext('NO LINE ALLOCATED')
                EB.ErrorProcessing.StoreEndError()
            END
            IF Y.OVERRIDE.ID EQ 'EXCESS.ID' THEN
                EB.SystemTables.setEtext('You have Excess over Limit')
                EB.ErrorProcessing.StoreEndError()
            END
        NEXT I
    END

RETURN
*** </region>

END
