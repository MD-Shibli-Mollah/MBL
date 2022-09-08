* @ValidationCode : Mjo0MTA1Njc4Nzc6Q3AxMjUyOjE2MjgwODQ2MjM5Nzg6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Aug 2021 19:43:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.V.AA.EXP.ACC
*-----------------------------------------------------------------------------
* Modification History :
*Subroutine Description: Validation routine for Expired Loan(AA or Acc Num)
*Subroutine Type:
*Attached To    :  ACTIVITY.API - MBL.ACCT.TITLE.API, Property Class: ACCOUNT, Action: MAINTAIN
*Attached As    : RECORD ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 03/08/2021 -                            Developed By   - MD. Shibli Mollah
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    
    $USING AA.Account
    $USING AC.AccountOpening
    $USING EB.SystemTables
    $USING AA.Framework
    $USING EB.Updates
    $USING EB.DataAccess

*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    FLD.POS = ''
RETURN
*-----------------------------------------------------------------------------

OPENFILES:

    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN
*-----------------------------------------------------------------------------

PROCESS:
* ACCOUNT.TITLE.1, SHORT.TITLE, ANNIVERSARY, LT.PROD.PREFIX, LT.AC.INIT.LMT, LT.AC.ACT.OPNDT, LT.AC.BD.LNMADT, LT.AC.BD.LN.AMT, LT.AC.BD.LNSTDT,LT.AC.BD.SNCADV,LT.TF.PURP.CODE,LT.AC.ELGBL.SEC, LT.AC.SECRTY.CD,LT.AC.NM.DT.EXP
    
*--------Map Local fields----------------------------------------------------------------
    APPLICATION.NAME = 'AA.ARRANGEMENT.ACTIVITY':FM:'AA.ARR.ACCOUNT'
    LOCAL.FIELDS = 'LT.EXP.LN.ID':@FM:'LT.PROD.PREFIX':@VM:'LT.AC.INIT.LMT':@VM:'LT.AC.ACT.OPNDT':@VM:'LT.AC.BD.LNMADT':@VM:'LT.AC.BD.LN.AMT':@VM:'LT.AC.BD.LNSTDT':@VM:'LT.AC.BD.SNCADV':@VM:'LT.TF.PURP.CODE':@VM:'LT.AC.ELGBL.SEC':@VM:'LT.AC.SECRTY.CD':@VM:'LT.AC.NM.DT.EXP'
*                         1                 1                    2                     3                      4                     5                     6                      7                    8                     9                    10                    11
    EB.Updates.MultiGetLocRef(APPLICATION.NAME, LOCAL.FIELDS, FLD.POS)
    Y.EXP.LN.ID.POS = FLD.POS<1,1>
    Y.LT.PROD.PREFIX.POS = FLD.POS<2,1>
    Y.LT.AC.INIT.LMT.POS = FLD.POS<2,2>
    Y.LT.AC.ACT.OPNDT.POS = FLD.POS<2,3>
    Y.LT.AC.BD.LNMADT.POS = FLD.POS<2,4>
    Y.LT.AC.BD.LN.AMT.POS = FLD.POS<2,5>
    Y.LT.AC.BD.LNSTDT.POS = FLD.POS<2,6>
    Y.LT.AC.BD.SNCADV.POS = FLD.POS<2,7>
    Y.LT.TF.PURP.CODE.POS = FLD.POS<2,8>
    Y.LT.AC.ELGBL.SEC.POS = FLD.POS<2,9>
    Y.LT.AC.SECRTY.CD.POS = FLD.POS<2,10>
    Y.LT.AC.NM.DT.EXP.POS = FLD.POS<2,11>
*--------END----------------------------------------------------------------

    Y.LOC.DATA = c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActLocalRef>
    Y.AA.ID = Y.LOC.DATA<1, Y.EXP.LN.ID.POS>
    
    Y.AA.LEN = Y.AA.ID[1,2]
*Y.AA.ID = c_aalocArrId

*-----------------------AA ID CHECK-----------------------------------------
    IF Y.AA.LEN NE 'AA' THEN
        Y.ACC = Y.AA.ID
    
        EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
* Y.CUS.ID = R.ACC<AC.AccountOpening.Account.Customer>
        Y.AA.ID = R.ACC<AC.AccountOpening.Account.ArrangementId>
    END
*---------------------------END-------------------------------------------
*---------------------------AA.ARR.ACCOUNT------------------------------------------
    PROP.CLASS.ACC = 'ACCOUNT'
    AA.Framework.GetArrangementConditions(Y.AA.ID, PROP.CLASS.ACC, PROPERTY, '', RETURN.IDS.ACC, RETURN.VALUES.ACC, ERR.MSG.ACC)
    R.ACC.DET= RAISE(RETURN.VALUES.ACC)
*Get Data
    Y.ACT.TITLE = R.ACC.DET<AA.Account.Account.AcAccountTitleOne>
    Y.SHORT.TITLE = R.ACC.DET<AA.Account.Account.AcShortTitle>
    Y.ANNIVERSARY = R.ACC.DET<AA.Account.Account.AcAnniversary>
    Y.LT.PROD.PREFIX = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.PROD.PREFIX.POS>
    Y.LT.AC.INIT.LMT = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.INIT.LMT.POS>
    Y.LT.AC.ACT.OPNDT = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.ACT.OPNDT.POS>
    Y.LT.AC.BD.LNMADT = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.BD.LNMADT.POS>
    Y.LT.AC.BD.LN.AMT = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.BD.LN.AMT.POS>
    Y.LT.AC.BD.LNSTDT = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.BD.LNSTDT.POS>
    Y.LT.AC.BD.SNCADV = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.BD.SNCADV.POS>
    Y.LT.TF.PURP.CODE = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.TF.PURP.CODE.POS>
    Y.LT.AC.ELGBL.SEC = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.ELGBL.SEC.POS>
    Y.LT.AC.SECRTY.CD = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.SECRTY.CD.POS>
    Y.LT.AC.NM.DT.EXP = R.ACC.DET<AA.Account.Account.AcLocalRef,Y.LT.AC.NM.DT.EXP.POS>
    
*set acc data
    EB.SystemTables.setRNew(AA.Account.Account.AcAccountTitleOne, Y.ACT.TITLE)
    EB.SystemTables.setRNew(AA.Account.Account.AcShortTitle, Y.SHORT.TITLE)
    EB.SystemTables.setRNew(AA.Account.Account.AcAnniversary, Y.ANNIVERSARY)
    
*-------SET LOCAL DATA------------------------------------------------------------------
    Y.TEMP = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
    Y.TEMP<1,Y.LT.PROD.PREFIX.POS> = Y.LT.PROD.PREFIX
    Y.TEMP<1,Y.LT.AC.INIT.LMT.POS> = Y.LT.AC.INIT.LMT
    Y.TEMP<1,Y.LT.AC.ACT.OPNDT.POS> = Y.LT.AC.ACT.OPNDT
    Y.TEMP<1,Y.LT.AC.BD.LNMADT.POS> = Y.LT.AC.BD.LNMADT
    Y.TEMP<1,Y.LT.AC.BD.LN.AMT.POS> = Y.LT.AC.BD.LN.AMT
    Y.TEMP<1,Y.LT.AC.BD.LNSTDT.POS> = Y.LT.AC.BD.LNSTDT
    Y.TEMP<1,Y.LT.AC.BD.SNCADV.POS> = Y.LT.AC.BD.SNCADV
    Y.TEMP<1,Y.LT.TF.PURP.CODE.POS> = Y.LT.TF.PURP.CODE
    Y.TEMP<1,Y.LT.AC.ELGBL.SEC.POS> = Y.LT.AC.ELGBL.SEC
    Y.TEMP<1,Y.LT.AC.SECRTY.CD.POS> = Y.LT.AC.SECRTY.CD
    Y.TEMP<1,Y.LT.AC.NM.DT.EXP.POS> = Y.LT.AC.NM.DT.EXP
        
    EB.SystemTables.setRNew(AA.Account.Account.AcLocalRef, Y.TEMP)
    
*-------SET LOCAL DATA----------------END--------------------------------------------------
**-------------------------------AA.ARR.LIMIT-----------------------------------------------
*    PROP.CLASS.LIM = 'LIMIT'
*    AA.Framework.GetArrangementConditions(Y.AA.ID, PROP.CLASS.LIM, PROPERTY, '', RETURN.IDS.LIM, RETURN.VALUES.LIM, ERR.MSG.LIM)
*    R.LIMIT = RAISE(RETURN.VALUES.LIM)
*    Y.LIM.REF = R.LIMIT<AA.Limit.Limit.LimLimitReference>
*    Y.LIM.SER = R.LIMIT<AA.Limit.Limit.LimLimitSerial>
*
**SET LIMIT DATA
*    EB.SystemTables.setRNew(AA.Limit.Limit.LimLimitReference, Y.LIM.REF)
*    EB.SystemTables.setRNew(AA.Limit.Limit.LimLimitSerial, Y.LIM.SER)
    
*-------------------------------TRACER-------------------------------------------------
*    WriteData = "Y.LT.PROD.PREFIX: ":Y.LT.PROD.PREFIX:" Y.LT.AC.INIT.LMT: ":Y.LT.AC.INIT.LMT:" Y.LT.AC.ACT.OPNDT: ":Y.LT.AC.ACT.OPNDT:" Y.LT.AC.BD.LNMADT: ":Y.LT.AC.BD.LNMADT:" Y.LT.AC.BD.LN.AMT :":Y.LT.AC.BD.LN.AMT:" Y.LT.AC.BD.LNSTDT: ":Y.LT.AC.BD.LNSTDT:" Y.LT.AC.BD.SNCADV: ":Y.LT.AC.BD.SNCADV:" Y.LT.TF.PURP.CODE: ":Y.LT.TF.PURP.CODE:" Y.LT.AC.ELGBL.SEC: ":Y.LT.AC.ELGBL.SEC:" Y.LT.AC.SECRTY.CD: ":Y.LT.AC.SECRTY.CD:" Y.LT.AC.NM.DT.EXP: ":Y.LT.AC.NM.DT.EXP
*    WriteData = Y.AA.ID:" R.LIMIT: ":R.LIMIT:" Y.LIM.REF: ":Y.LIM.REF:" Y.LIM.SER: ":Y.LIM.SER
*    FileName = 'SHIBLI_CR.AA.LIMIT.txt'
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

*-------------------------------TRACER END-------------------------------------------------


    
RETURN
********************************************************
END
