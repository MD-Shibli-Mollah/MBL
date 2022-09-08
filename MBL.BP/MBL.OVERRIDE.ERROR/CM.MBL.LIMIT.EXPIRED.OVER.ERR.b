SUBROUTINE CM.MBL.LIMIT.EXPIRED.OVER.ERR
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*Subroutine Description: this routine create for make limit 2(LIMIT.EXPIRED) ovrride message into error message.
*Subroutine Type:
*Attached To    : TT, FT versions Control.
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
       
    $USING TT.Contract
    $USING FT.Contract
  
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
   
PROCESS:

****************FOR CATCH OFS OVERRIDE ERROR Start*************
    Y.FLD.OVERRIDE = ''
    Y.OVERRIDE.COUNT = 0
    Y.FLD.OVERRIDE.ID = ''
    Y.FLD.OVERRIDE = EB.SystemTables.getRNew(V - 9) ;*FOR GET THIS OVERRIDE NEED OVERRIDE FIELD ADDED IN VERSION LABLE.
    Y.OVERRIDE.COUNT = DCOUNT(Y.FLD.OVERRIDE, '}')
    FOR U = 1 TO Y.OVERRIDE.COUNT
        Y.FLD.OVERRIDE.ID = FIELD(Y.FLD.OVERRIDE, '}',U)
        IF Y.FLD.OVERRIDE.ID EQ 'LIMIT.EXPIRED' THEN
            EB.SystemTables.setEtext('Account Limit has been Expired')
            EB.ErrorProcessing.StoreEndError()
        END
    NEXT U
****************FOR CATCH OFS OVERRIDE ERROR End**************

    Y.DR.ACCT = ''
    Y.TNX.TYPE = ''
    Y.WORK.BAL = ''
    Y.APP = EB.SystemTables.getApplication()
    IF Y.APP EQ 'TELLER' OR Y.APP EQ 'FUNDS.TRANSFER' THEN
        IF Y.APP EQ 'FUNDS.TRANSFER' THEN
            Y.TNX.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType)
            Y.DR.ACCT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        END ELSE
            Y.DR.ACCT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
        END
        IF Y.TNX.TYPE EQ 'ACPO' THEN RETURN
        IF Y.DR.ACCT EQ '' THEN RETURN
        AA.Account.GetActArrId(Y.DR.ACCT, Flag, RAccount, ReturnError)
        Y.WORK.BAL =  RAccount<AC.AccountOpening.Account.WorkingBalance>
        IF Y.WORK.BAL NE '' THEN
            IF Y.WORK.BAL GT 0 THEN RETURN
        END
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
  
RETURN
      
ERROR.PROCESS:
    Y.OVERRIDE.ID.LIST = ''
    Y.OVERRIDE.ID.LIST = EB.OverrideProcessing.getOveOverrideId()
    Y.COUNT = DCOUNT(Y.OVERRIDE.ID.LIST,VM)
    FOR I=1 TO Y.COUNT
        IF Y.OVERRIDE.ID.LIST<1,I> = 'LIMIT.EXPIRED' THEN
            IF Y.APP EQ 'FUNDS.TRANSFER' THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
            END
            IF Y.APP EQ 'TELLER' THEN
                EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
            END
            EB.SystemTables.setEtext('Account Limit has been Expired')
            EB.ErrorProcessing.StoreEndError()
        END
    NEXT I

    Y.VM.COUNT = DCOUNT(OFS$OVERRIDES, @VM)
    IF Y.VM.COUNT THEN
        FOR I = 1 TO Y.VM.COUNT
            Y.FMT.1 = FIELD(OFS$OVERRIDES<1,I>," ",1)
            Y.FMT.3 = FIELD(OFS$OVERRIDES<1,I>," ",3)
            IF Y.FMT.1 EQ "LIMIT" AND Y.FMT.3 EQ "EXPIRED" THEN
                IF Y.APP EQ 'FUNDS.TRANSFER' THEN
                    EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
                END
                IF Y.APP EQ 'TELLER' THEN
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                END
                EB.SystemTables.setEtext('Account Limit has been Expired')
                EB.ErrorProcessing.StoreEndError()
            END
        NEXT I
    END
    Y.SOA.OVERRIDES.COUNT = DCOUNT(SOA$OVERRIDES,'"')
    IF Y.SOA.OVERRIDES.COUNT THEN
        FOR I = 2 TO Y.SOA.OVERRIDES.COUNT STEP 2
            Y.OVERRIDE.ID = FIELD(SOA$OVERRIDES,'"',I)
            IF Y.OVERRIDE.ID EQ 'LIMIT.EXPIRED' THEN
                IF Y.APP EQ 'FUNDS.TRANSFER' THEN
                    EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
                END
                IF Y.APP EQ 'TELLER' THEN
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                END
                EB.SystemTables.setEtext('Account Limit has been Expired')
                EB.ErrorProcessing.StoreEndError()
            END
        NEXT I
    END
  
RETURN

END
