* @ValidationCode : MjotNjIzNjcyNTc1OkNwMTI1MjoxNjI3Mzc3NTExNDI4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Jul 2021 15:18:31
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

SUBROUTINE CR.MBL.E.NOF.LOAN.LISTING(Y.DATA)
*-----------------------------------------------------------------------------
*
    $INSERT  I_COMMON
    $INSERT  I_EQUATE

    $USING EB.Reports
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING AA.Framework
    $USING RE.ConBalanceUpdates
    $USING AA.Account
    $USING AA.Limit
    $USING ST.Customer
    $USING LI.Config
    $USING AA.Interest
    $USING ST.CompanyCreation
    $USING AA.PaymentSchedule
    $USING AA.ProductManagement
    $USING ST.Config
    $USING AA.TermAmount
    $USING EB.Updates
    
    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
RETURN

*============*
INTT:
*============*

    Y.COMPANY = EB.SystemTables.getIdCompany()
    
    FN.CUS="F.CUSTOMER"
    F.CUS=""
    
    FN.BILL.DETAILS= "F.AA.BILL.DETAILS"
    F.BILL.DETAILS = ""
    
    FN.CUSTOMER.ACCOUNT = "F.CUSTOMER.ACCOUNT"
    F.CUSTOMER.ACCOUNT = ""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
    FN.AA.PRODUCT="F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.ACCT.DETAILS = "F.AA.ACCOUNT.DETAILS"
    F.ACCT.DETAILS = ""
    
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    
    FN.AA.ARR.ACCT = "F.AA.ARR.ACCOUNT"
    F.AA.ARR.ACCT =""
    
    FN.AA.INT.ACCR = "F.AA.INTEREST.ACCRUALS"
    F.AA.INT.ACCR = ""
    
    FN.AA.ARR.PAY.SCH = "F.AA.ARR.PAYMENT.SCHEDULE"
    F.AA.ARR.PAY.SCH = ""
    
    FN.LIMIT="F.AA.ARR.LIMIT"
    F.LIMIT=""
    
    FN.LIM="F.LIMIT"
    F.LIM=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
    FN.CAT = 'F.CATEGORY'
    F.CAT = ''
    
    Y.LN.LC.NUM = ''
    Y.CAT.DES = ''
    Y.VALUE.DATE = ''
    Y.CUR.BAL = ''
    Y.PR.AMT = ''
    Y.STATUS = ''
    Y.FC.AMT = ''
    Y.INS.FREQ = ''

    Y.OUT.DR = ''
    Y.OUT.CR = ''
    Y.SYS.SUS.AMT = ''
    Y.MAN.SUS.AMT = ''
    Y.DIS.AMT = ''
    Y.PRD.DES = ''
    
    APPLICATION.NAMES = 'AA.ARR.ACCOUNT':FM:'LIMIT.REFERENCE'
    LOCAL.FIELDS = 'LT.AC.BD.LNSTDT':VM:'LT.AC.BD.LNMADT':VM:'LT.ASSET.CLASS':FM:'LT.LN.TYPE'
    EB.Updates.MultiGetLocRef(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
    Y.LT.AC.BD.LNSTDT = FLD.POS<1,1>
    Y.LT.LN.TYPE.POS = FLD.POS<2,1>
    Y.AC.BD.LNMADT.POS = FLD.POS<1,2>
    Y.LT.ASSET.CLASS.POSS = FLD.POS<1,3>
     
RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.CUSTOMER.ACCOUNT, F.CUSTOMER.ACCOUNT)
    EB.DataAccess.Opf(FN.BILL.DETAILS, F.BILL.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARR.ACCT, F.AA.ARR.ACCT)
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.LIM,F.LIM)
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.AA.PRODUCT,F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.AA.INT.ACCR, F.AA.INT.ACCR)
    EB.DataAccess.Opf(FN.AA.ARR.PAY.SCH, F.AA.ARR.PAY.SCH)
    
RETURN

PROCESS:

*---------------------------------------------------------------------
    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.OD.PROPER = "INTONOD"
        Y.INT.PROPER = "PRINCIPALINT"
        Y.CR.ACC = 'CURACCOUNT'
        Y.PR.INT = 'ACCPRINCIPALINT'
        Y.DUE.PR.INT = 'DUEPRINCIPALINT'
        Y.DUE.ACC = "DUEACCOUNT"
*MUST INCLUDE IN PRINCIPAL AMT--- DUEACCOUNT****
        Y.PD.BAL.TYPE.ALL = 'DELACCOUNT':VM:'DOFACCOUNT':VM:'GRCACCOUNT':VM:'NABACCOUNT':VM:'SMAACCOUNT':VM:'STDACCOUNT':VM:'SUBACCOUNT'
        Y.PD.PFT.ALL = 'ACCINTONOD':VM:'DELPRINCIPALINT':VM:'DOFPRINCIPALINT':VM:'GRCPRINCIPALINT':VM:'NABPRINCIPALINT':VM:'SMAPRINCIPALINT':VM:'STDPRINCIPALINT':VM:'SUBPRINCIPALINT'
    
*----------------------------Main Process--------------------------------------------------
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH (PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS') AND (PRODUCT.GROUP EQ 'MBL.BLOCK.INTER.LN' 'MBL.CAR.PRN.LN' 'MBL.CSHCR.AG.LN' 'MBL.CSHCR.HP.LN' 'MBL.EDF.INFIN.LN' 'MBL.EDF.LN' 'MBL.EHBL.PRN.LN' 'MBL.LIQ.AC.LN' 'MBL.PAD.BTB.LN' 'MBL.PAD.CASH.LN' 'MBL.PRV.FND.LN' 'MBL.SOD.AGR.LN' 'MBL.SOD.EMFS.LN' 'MBL.SOD.FDR.LN' 'MBL.SOD.GEN.LN' 'MBL.SOD.SCHM.LN' 'MBL.ST.PRS.P.LN' 'IS.MBL.AGR.HPSM.LN' 'IS.MBL.AGR.MUAJ.LN' 'IS.MBL.COR.ASRF.LN' 'IS.MBL.COR.HPSM.LN' 'IS.MBL.COR.MUAJ.LN' 'IS.MBL.COR.MUR.LN' 'IS.MBL.COR.MUSH.LN' 'IS.MBL.COR.QRD.LN' 'IS.MBL.COR.SALM.LN' 'IS.MBL.MIC.HPSM.LN' 'IS.MBL.MIC.MUAJ.LN' 'IS.MBL.MIC.MUR.LN' 'IS.MBL.MIC.MUSH.LN' 'IS.MBL.MIC.QRD.LN' 'IS.MBL.RET.HPSM.LN' 'IS.MBL.RET.MUAJ.LN' 'IS.MBL.RET.QRD.LN' 'IS.MBL.SME.ASRF.LN' 'IS.MBL.SME.HPSM.LN' 'IS.MBL.SME.MUAJ.LN' 'IS.MBL.SME.MUR.LN' 'IS.MBL.SME.MUSH.LN' 'IS.MBL.SME.QRD.LN' 'IS.MBL.SME.SALM.LN' 'MBL.ANY.PURP.LN' 'MBL.AUTO.RET.LN' 'MBL.BB.RF.FC.LN' 'MBL.BBREFIN.GRP.LN' 'MBL.BIL.DISC.LN' 'MBL.BILLDISC.GRP.LN' 'MBL.COTT.RET.LN' 'MBL.DOCT.RET.LN' 'MBL.EDU.RET.LN' 'MBL.FBP.LN' 'MBL.FDBP.GRP.LN' 'MBL.FDBP.LN' 'MBL.FO.LN' 'MBL.FORCE.BG.LN' 'MBL.FORCE.IMP.LN' 'MBL.GEN.CORP.LN' 'MBL.HBL.LN' 'MBL.HIRE.PUR.LN' 'MBL.HOUSE.FU.LN' 'MBL.HREPRCH.GRP.LN' 'MBL.HSBLDNG.GRP.LN' 'MBL.IBP.CLN' 'MBL.IBPCLN.GRP.LN' 'MBL.IDBP.GRP.LN' 'MBL.IDBP.LN' 'MBL.LEAS.AGR.LN' 'MBL.LEAS.FIN.LN' 'MBL.LEASEFIN.GRP.LN' 'MBL.LOANFO.GRP.LN' 'MBL.LTR.GRP.LN' 'MBL.LTR.LN' 'MBL.OTH.CONS.LN' 'MBL.OTHER.LN' 'MBL.OTHLN.GRP.LN' 'MBL.OTHRETL.GRP.LN' 'MBL.OVRS.EMP.LN' 'MBL.PACK.CR.LN' 'MBL.PC.GRP.LN' 'MBL.PER.RET.LN' 'MBL.PRANTIK.LN' 'MBL.SHRT.AGR.LN' 'MBL.SHRT.TRM.LN' 'MBL.SHRTLN.GRP.LN' 'MBL.SML.SCH.LN' 'MBL.SOD.WO.LN' 'MBL.SODWO.GRP.LN' 'MBL.TERM.AGR.LN' 'MBL.TERM.FIS.LN' 'MBL.TERM.LN' 'MBL.TIME.FIS.LN' 'MBL.TIME.FRC.LN' 'MBL.TIME.LN' 'MBL.TIMELN.GRP.LN' 'MBL.TL.AGRI.REF.LN' 'MBL.TL.FSF.LN' 'MBL.TRM.NGO.LN' 'MBL.TRMLN.GRP.LN') AND (ARR.STATUS EQ 'AUTH' 'CURRENT') AND CO.CODE EQ ":Y.COMPANY
*--------HEAD OFFICE--javascript:doloadCompany('BD0010001');------------------------------------
        IF Y.COMPANY EQ 'BD0010001' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH (PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS') AND (PRODUCT.GROUP EQ 'MBL.BLOCK.INTER.LN' 'MBL.CAR.PRN.LN' 'MBL.CSHCR.AG.LN' 'MBL.CSHCR.HP.LN' 'MBL.EDF.INFIN.LN' 'MBL.EDF.LN' 'MBL.EHBL.PRN.LN' 'MBL.LIQ.AC.LN' 'MBL.PAD.BTB.LN' 'MBL.PAD.CASH.LN' 'MBL.PRV.FND.LN' 'MBL.SOD.AGR.LN' 'MBL.SOD.EMFS.LN' 'MBL.SOD.FDR.LN' 'MBL.SOD.GEN.LN' 'MBL.SOD.SCHM.LN' 'MBL.ST.PRS.P.LN' 'IS.MBL.AGR.HPSM.LN' 'IS.MBL.AGR.MUAJ.LN' 'IS.MBL.COR.ASRF.LN' 'IS.MBL.COR.HPSM.LN' 'IS.MBL.COR.MUAJ.LN' 'IS.MBL.COR.MUR.LN' 'IS.MBL.COR.MUSH.LN' 'IS.MBL.COR.QRD.LN' 'IS.MBL.COR.SALM.LN' 'IS.MBL.MIC.HPSM.LN' 'IS.MBL.MIC.MUAJ.LN' 'IS.MBL.MIC.MUR.LN' 'IS.MBL.MIC.MUSH.LN' 'IS.MBL.MIC.QRD.LN' 'IS.MBL.RET.HPSM.LN' 'IS.MBL.RET.MUAJ.LN' 'IS.MBL.RET.QRD.LN' 'IS.MBL.SME.ASRF.LN' 'IS.MBL.SME.HPSM.LN' 'IS.MBL.SME.MUAJ.LN' 'IS.MBL.SME.MUR.LN' 'IS.MBL.SME.MUSH.LN' 'IS.MBL.SME.QRD.LN' 'IS.MBL.SME.SALM.LN' 'MBL.ANY.PURP.LN' 'MBL.AUTO.RET.LN' 'MBL.BB.RF.FC.LN' 'MBL.BBREFIN.GRP.LN' 'MBL.BIL.DISC.LN' 'MBL.BILLDISC.GRP.LN' 'MBL.COTT.RET.LN' 'MBL.DOCT.RET.LN' 'MBL.EDU.RET.LN' 'MBL.FBP.LN' 'MBL.FDBP.GRP.LN' 'MBL.FDBP.LN' 'MBL.FO.LN' 'MBL.FORCE.BG.LN' 'MBL.FORCE.IMP.LN' 'MBL.GEN.CORP.LN' 'MBL.HBL.LN' 'MBL.HIRE.PUR.LN' 'MBL.HOUSE.FU.LN' 'MBL.HREPRCH.GRP.LN' 'MBL.HSBLDNG.GRP.LN' 'MBL.IBP.CLN' 'MBL.IBPCLN.GRP.LN' 'MBL.IDBP.GRP.LN' 'MBL.IDBP.LN' 'MBL.LEAS.AGR.LN' 'MBL.LEAS.FIN.LN' 'MBL.LEASEFIN.GRP.LN' 'MBL.LOANFO.GRP.LN' 'MBL.LTR.GRP.LN' 'MBL.LTR.LN' 'MBL.OTH.CONS.LN' 'MBL.OTHER.LN' 'MBL.OTHLN.GRP.LN' 'MBL.OTHRETL.GRP.LN' 'MBL.OVRS.EMP.LN' 'MBL.PACK.CR.LN' 'MBL.PC.GRP.LN' 'MBL.PER.RET.LN' 'MBL.PRANTIK.LN' 'MBL.SHRT.AGR.LN' 'MBL.SHRT.TRM.LN' 'MBL.SHRTLN.GRP.LN' 'MBL.SML.SCH.LN' 'MBL.SOD.WO.LN' 'MBL.SODWO.GRP.LN' 'MBL.TERM.AGR.LN' 'MBL.TERM.FIS.LN' 'MBL.TERM.LN' 'MBL.TIME.FIS.LN' 'MBL.TIME.FRC.LN' 'MBL.TIME.LN' 'MBL.TIMELN.GRP.LN' 'MBL.TL.AGRI.REF.LN' 'MBL.TL.FSF.LN' 'MBL.TRM.NGO.LN' 'MBL.TRMLN.GRP.LN') AND (ARR.STATUS EQ 'AUTH' 'CURRENT')"
        END
    END
    
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.OD.PROPER = "PFTONOD"
        Y.INT.PROPER = "DEFERREDPFT"
        Y.CR.ACC = 'CURISACCOUNT'
        Y.PR.INT = ''
        Y.DUE.PR.INT = 'DUEDEFERREDPFT'
* Y.MRK.PR.INT = 'RECMARKUPPFT'
        Y.DUE.MRK.PR.INT = 'DUEMARKUPPFT'
        Y.DUE.ACC = "DUEISACCOUNT"
*MUST INCLUDE IN PRINCIPAL AMT--- DUEISACCOUNT****
        Y.PD.BAL.TYPE.ALL = 'DELISACCOUNT':VM:'DOFISACCOUNT':VM:'GRCISACCOUNT':VM:'NABISACCOUNT':VM:'SMAISACCOUNT':VM:'STDISACCOUNT':VM:'SUBISACCOUNT'
        Y.MARK.DEF.PFT.ALL = 'ACCDEFERREDPFT':VM:'DELDEFERREDPFT':VM:'DOFDEFERREDPFT':VM:'GRCDEFERREDPFT':VM:'NABDEFERREDPFT':VM:'SMADEFERREDPFT':VM:'STDDEFERREDPFT':VM:'SUBDEFERREDPFT':VM:'RECMARKUPPFT':VM:'STDMARKUPPFT':VM:'DELMARKUPPFT':VM:'DOFMARKUPPFT':VM:'GRCMARKUPPFT':VM:'NABMARKUPPFT':VM:'SMAMARKUPPFT':VM:'SUBMARKUPPFT'
        Y.PENAL.PFT.ALL = 'ACCPENALTYPFT'
*-----------------Pft on OD IS ONLY ACCPFTONOD--------------------------****
        Y.PFT.OD.BAL.TYPE = 'ACCPFTONOD'
*---------------------MARKUP/DEFERRED PFT ALL BAL TYPE----------------------***
        Y.PD.PFT.ALL = Y.MARK.DEF.PFT.ALL
*----------------------------Main Process--------------------------------------------------
    
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH (PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS') AND (PRODUCT.GROUP EQ 'MBL.BLOCK.INTER.LN' 'MBL.CAR.PRN.LN' 'MBL.CSHCR.AG.LN' 'MBL.CSHCR.HP.LN' 'MBL.EDF.INFIN.LN' 'MBL.EDF.LN' 'MBL.EHBL.PRN.LN' 'MBL.LIQ.AC.LN' 'MBL.PAD.BTB.LN' 'MBL.PAD.CASH.LN' 'MBL.PRV.FND.LN' 'MBL.SOD.AGR.LN' 'MBL.SOD.EMFS.LN' 'MBL.SOD.FDR.LN' 'MBL.SOD.GEN.LN' 'MBL.SOD.SCHM.LN' 'MBL.ST.PRS.P.LN' 'IS.MBL.AGR.HPSM.LN' 'IS.MBL.AGR.MUAJ.LN' 'IS.MBL.COR.ASRF.LN' 'IS.MBL.COR.HPSM.LN' 'IS.MBL.COR.MUAJ.LN' 'IS.MBL.COR.MUR.LN' 'IS.MBL.COR.MUSH.LN' 'IS.MBL.COR.QRD.LN' 'IS.MBL.COR.SALM.LN' 'IS.MBL.MIC.HPSM.LN' 'IS.MBL.MIC.MUAJ.LN' 'IS.MBL.MIC.MUR.LN' 'IS.MBL.MIC.MUSH.LN' 'IS.MBL.MIC.QRD.LN' 'IS.MBL.RET.HPSM.LN' 'IS.MBL.RET.MUAJ.LN' 'IS.MBL.RET.QRD.LN' 'IS.MBL.SME.ASRF.LN' 'IS.MBL.SME.HPSM.LN' 'IS.MBL.SME.MUAJ.LN' 'IS.MBL.SME.MUR.LN' 'IS.MBL.SME.MUSH.LN' 'IS.MBL.SME.QRD.LN' 'IS.MBL.SME.SALM.LN' 'MBL.ANY.PURP.LN' 'MBL.AUTO.RET.LN' 'MBL.BB.RF.FC.LN' 'MBL.BBREFIN.GRP.LN' 'MBL.BIL.DISC.LN' 'MBL.BILLDISC.GRP.LN' 'MBL.COTT.RET.LN' 'MBL.DOCT.RET.LN' 'MBL.EDU.RET.LN' 'MBL.FBP.LN' 'MBL.FDBP.GRP.LN' 'MBL.FDBP.LN' 'MBL.FO.LN' 'MBL.FORCE.BG.LN' 'MBL.FORCE.IMP.LN' 'MBL.GEN.CORP.LN' 'MBL.HBL.LN' 'MBL.HIRE.PUR.LN' 'MBL.HOUSE.FU.LN' 'MBL.HREPRCH.GRP.LN' 'MBL.HSBLDNG.GRP.LN' 'MBL.IBP.CLN' 'MBL.IBPCLN.GRP.LN' 'MBL.IDBP.GRP.LN' 'MBL.IDBP.LN' 'MBL.LEAS.AGR.LN' 'MBL.LEAS.FIN.LN' 'MBL.LEASEFIN.GRP.LN' 'MBL.LOANFO.GRP.LN' 'MBL.LTR.GRP.LN' 'MBL.LTR.LN' 'MBL.OTH.CONS.LN' 'MBL.OTHER.LN' 'MBL.OTHLN.GRP.LN' 'MBL.OTHRETL.GRP.LN' 'MBL.OVRS.EMP.LN' 'MBL.PACK.CR.LN' 'MBL.PC.GRP.LN' 'MBL.PER.RET.LN' 'MBL.PRANTIK.LN' 'MBL.SHRT.AGR.LN' 'MBL.SHRT.TRM.LN' 'MBL.SHRTLN.GRP.LN' 'MBL.SML.SCH.LN' 'MBL.SOD.WO.LN' 'MBL.SODWO.GRP.LN' 'MBL.TERM.AGR.LN' 'MBL.TERM.FIS.LN' 'MBL.TERM.LN' 'MBL.TIME.FIS.LN' 'MBL.TIME.FRC.LN' 'MBL.TIME.LN' 'MBL.TIMELN.GRP.LN' 'MBL.TL.AGRI.REF.LN' 'MBL.TL.FSF.LN' 'MBL.TRM.NGO.LN' 'MBL.TRMLN.GRP.LN') AND (ARR.STATUS EQ 'AUTH' 'CURRENT') AND CO.CODE EQ ":Y.COMPANY
*--------HEAD OFFICE--javascript:doloadCompany('BD0010001');------------------------------------
        IF Y.COMPANY EQ 'BD0010001' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH (PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS') AND (PRODUCT.GROUP EQ 'MBL.BLOCK.INTER.LN' 'MBL.CAR.PRN.LN' 'MBL.CSHCR.AG.LN' 'MBL.CSHCR.HP.LN' 'MBL.EDF.INFIN.LN' 'MBL.EDF.LN' 'MBL.EHBL.PRN.LN' 'MBL.LIQ.AC.LN' 'MBL.PAD.BTB.LN' 'MBL.PAD.CASH.LN' 'MBL.PRV.FND.LN' 'MBL.SOD.AGR.LN' 'MBL.SOD.EMFS.LN' 'MBL.SOD.FDR.LN' 'MBL.SOD.GEN.LN' 'MBL.SOD.SCHM.LN' 'MBL.ST.PRS.P.LN' 'IS.MBL.AGR.HPSM.LN' 'IS.MBL.AGR.MUAJ.LN' 'IS.MBL.COR.ASRF.LN' 'IS.MBL.COR.HPSM.LN' 'IS.MBL.COR.MUAJ.LN' 'IS.MBL.COR.MUR.LN' 'IS.MBL.COR.MUSH.LN' 'IS.MBL.COR.QRD.LN' 'IS.MBL.COR.SALM.LN' 'IS.MBL.MIC.HPSM.LN' 'IS.MBL.MIC.MUAJ.LN' 'IS.MBL.MIC.MUR.LN' 'IS.MBL.MIC.MUSH.LN' 'IS.MBL.MIC.QRD.LN' 'IS.MBL.RET.HPSM.LN' 'IS.MBL.RET.MUAJ.LN' 'IS.MBL.RET.QRD.LN' 'IS.MBL.SME.ASRF.LN' 'IS.MBL.SME.HPSM.LN' 'IS.MBL.SME.MUAJ.LN' 'IS.MBL.SME.MUR.LN' 'IS.MBL.SME.MUSH.LN' 'IS.MBL.SME.QRD.LN' 'IS.MBL.SME.SALM.LN' 'MBL.ANY.PURP.LN' 'MBL.AUTO.RET.LN' 'MBL.BB.RF.FC.LN' 'MBL.BBREFIN.GRP.LN' 'MBL.BIL.DISC.LN' 'MBL.BILLDISC.GRP.LN' 'MBL.COTT.RET.LN' 'MBL.DOCT.RET.LN' 'MBL.EDU.RET.LN' 'MBL.FBP.LN' 'MBL.FDBP.GRP.LN' 'MBL.FDBP.LN' 'MBL.FO.LN' 'MBL.FORCE.BG.LN' 'MBL.FORCE.IMP.LN' 'MBL.GEN.CORP.LN' 'MBL.HBL.LN' 'MBL.HIRE.PUR.LN' 'MBL.HOUSE.FU.LN' 'MBL.HREPRCH.GRP.LN' 'MBL.HSBLDNG.GRP.LN' 'MBL.IBP.CLN' 'MBL.IBPCLN.GRP.LN' 'MBL.IDBP.GRP.LN' 'MBL.IDBP.LN' 'MBL.LEAS.AGR.LN' 'MBL.LEAS.FIN.LN' 'MBL.LEASEFIN.GRP.LN' 'MBL.LOANFO.GRP.LN' 'MBL.LTR.GRP.LN' 'MBL.LTR.LN' 'MBL.OTH.CONS.LN' 'MBL.OTHER.LN' 'MBL.OTHLN.GRP.LN' 'MBL.OTHRETL.GRP.LN' 'MBL.OVRS.EMP.LN' 'MBL.PACK.CR.LN' 'MBL.PC.GRP.LN' 'MBL.PER.RET.LN' 'MBL.PRANTIK.LN' 'MBL.SHRT.AGR.LN' 'MBL.SHRT.TRM.LN' 'MBL.SHRTLN.GRP.LN' 'MBL.SML.SCH.LN' 'MBL.SOD.WO.LN' 'MBL.SODWO.GRP.LN' 'MBL.TERM.AGR.LN' 'MBL.TERM.FIS.LN' 'MBL.TERM.LN' 'MBL.TIME.FIS.LN' 'MBL.TIME.FRC.LN' 'MBL.TIME.LN' 'MBL.TIMELN.GRP.LN' 'MBL.TL.AGRI.REF.LN' 'MBL.TL.FSF.LN' 'MBL.TRM.NGO.LN' 'MBL.TRMLN.GRP.LN') AND (ARR.STATUS EQ 'AUTH' 'CURRENT')"
        END
    END
 
    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
    LOOP
        REMOVE Y.ARR.ID FROM SEL.LIST SETTING POS
    WHILE Y.ARR.ID:POS
*Loan ID
*     Y.ARR.ID = "AA20364QWN6C"
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.AA, F.AA.ARRANGEMENT, ERR.ARR)
        Y.LN.LC.NUM = Y.ARR.ID
        Y.CUS.ID = REC.AA<AA.Framework.Arrangement.ArrCustomer>
        Y.ACC.NUM = REC.AA<AA.Framework.Arrangement.ArrLinkedApplId>
        Y.PROD.LINE = REC.AA<AA.Framework.Arrangement.ArrProductLine>
        Y.PRD.GRP = REC.AA<AA.Framework.Arrangement.ArrProductGroup>
        Y.CURRENCY = REC.AA<AA.Framework.Arrangement.ArrCurrency>
        Y.ARR.STATUS = REC.AA<AA.Framework.Arrangement.ArrArrStatus>
        Y.ARR.COM.CODE = REC.AA<AA.Framework.Arrangement.ArrCoCode>
        Y.ARR.ST.DT = REC.AA<AA.Framework.Arrangement.ArrStartDate>
        
*-----------------------------------------------------------------------------
*----------------------------------END----------------------------------------------------
*
        EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACCT.DET, F.ACCT.DETAILS, Er)
        Y.ACC.VALUE.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBaseDate>
        Y.EXPIRY.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
        Y.TOT.BILL.TYPE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillType>
        Y.TOT.BILL.ID = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
        Y.STATUS = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdArrAgeStatus>
        
*-----------------------INSTALLMENT AMOUNT READ-----------------------------------
        CONVERT SM TO VM IN Y.TOT.BILL.TYPE
        CONVERT SM TO VM IN Y.TOT.BILL.ID
        Y.DCOUNT = DCOUNT(Y.TOT.BILL.TYPE,@VM)
*
        FOR J=1 TO Y.DCOUNT
            Y.BILL.TYPE = Y.TOT.BILL.TYPE<1,J>
            IF Y.BILL.TYPE EQ 'INSTALLMENT' THEN
                Y.BILL.ID = Y.TOT.BILL.ID<1,J>
                EB.DataAccess.FRead(FN.BILL.DETAILS, Y.BILL.ID, REC.AA.BILL.DET, F.BILL.DETAILS, ERR.AA)
                Y.TOT.BILL.TYPE = REC.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdBillType>
                Y.TOT.INS.AMT = REC.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdPaymentAmount>

                CONVERT SM TO VM IN Y.TOT.BILL.TYPE
                CONVERT SM TO VM IN Y.TOT.INS.AMT
                Y.DCOUNT = DCOUNT(Y.TOT.BILL.TYPE,@VM)
                
                FOR K=1 TO Y.DCOUNT
                    Y.BILL.TYPE = Y.TOT.BILL.TYPE<1,K>
                    IF Y.BILL.TYPE EQ 'INSTALLMENT' THEN
                        Y.INS.AMT = Y.TOT.INS.AMT<1,K>
                    END
                NEXT K
            END
        NEXT J
 
*-----------------------COMPANY/BRANCH NAME-------------------------------------------
        EB.DataAccess.FRead(FN.COM, Y.ARR.COM.CODE, R.COM, F.COM, C.ERR)
        Y.COM.NAME=R.COM<ST.CompanyCreation.Company.EbComCompanyName>
    
*-----------------------------------------------------------------------------
*Customer Name
    
        EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, R.CUS, F.CUS, CUS.ERR)
        Y.CUS.TITLE= R.CUS<ST.Customer.Customer.EbCusShortName>
    
*---------------------------------------------------------------------------------------
*Limit Ref.
        Y.PROP.CLS = 'LIMIT'
        AA.Framework.GetArrangementConditions(Y.ARR.ID,Y.PROP.CLS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        R.REC.LIMIT = RAISE(RETURN.VALUES)
        Y.LIMIT.REF = R.REC.LIMIT<AA.Limit.Limit.LimLimitReference>
        Y.LIMIT.SER = R.REC.LIMIT<AA.Limit.Limit.LimLimitSerial>
    
        Y.LIMIT = Y.LIMIT.REF:".":Y.LIMIT.SER
    
*---------------------------------------------------------------------------------------
*Loan Type
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.LN.LC.NUM, R.ARR, F.AA.ARRANGEMENT, LN.ERR.ARR)
        Y.LN.TYP = R.ARR<AA.Framework.Arrangement.ArrProduct>
    
        EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.TYP, PRD.REC, F.AA.PRODUCT, PRD.ERR)
        Y.PRD.DES = PRD.REC<AA.ProductManagement.Product.PdtDescription>
*---------------------------------------------------------------------------------------

*Sanction Date
        Y.LN.ID = Y.CUS.ID:".":"000":Y.LIMIT
        EB.DataAccess.FRead(FN.LIM, Y.LN.ID, R.ARGMNT, F.LIM, LIM.ERR)
*Limit Amount
        Y.LIM.AMT = R.ARGMNT<LI.Config.Limit.InternalAmount>
*----------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------
*OD Rate

        PROP.CLASS = 'INTEREST'
        PROPERTY= Y.OD.PROPER
        AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        R.REC = RAISE(RETURN.VALUES)
        Y.OD.INT.RATE = R.REC<AA.Interest.Interest.IntEffectiveRate>

*----------------------------------------------------------------------------------------------
*Regular int. Rate

        PROPERTY.P= Y.INT.PROPER
        AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY.P,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        PR.REC = RAISE(RETURN.VALUES)
        Y.PR.INT.RATE = PR.REC<AA.Interest.Interest.IntEffectiveRate>
        
        IF Y.PR.INT.RATE EQ '' THEN
            PROPERTY.P= "MARKUPPFT"
            AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY.P,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
            PR.REC = RAISE(RETURN.VALUES)
            Y.PR.INT.RATE = PR.REC<AA.Interest.Interest.IntEffectiveRate>
        END
*-----------------------------------------------------------------------------------------------

*
*Regular Outstanding
        Y.CUR.ACC = Y.CR.ACC
        Y.TODAY=EB.SystemTables.getToday()
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.ACC, Y.TODAY, TOT.CUR.AMT, RetError)
        Y.CUR.BAL = TOT.CUR.AMT
*------------------DUEACCOUNT/DUEISACCOUNT MUST BE CONSIDERED IN PR AMT-------------------
        Y.CUR.DUE.ACC = Y.DUE.ACC
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.DUE.ACC, Y.TODAY, TOT.DUE.AMT, RetError.2)
        Y.CUR.DUE.BAL = TOT.DUE.AMT

*--------------------------PFT ON OD ----------------------------------
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PFT.OD.BAL.TYPE, Y.TODAY, Y.PFT.OD.AMT, RetError4)
        Y.PFT.OD = Y.PFT.OD.AMT
       
*-------------------------------------Y.PD.PFT.ALL--------Y.MARK.DEF.PFT.ALL--------------------------------------

        Y.TOT.PD.PFT.DCOUNT = DCOUNT(Y.PD.PFT.ALL,VM)
    
        FOR I=1 TO Y.TOT.PD.PFT.DCOUNT
            Y.PD.PFT = Y.PD.PFT.ALL<1,I>
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PD.PFT, Y.TODAY, TOT.PD.PFT, RetError2)
            Y.PD.PFT.AMT = TOT.PD.PFT
            Y.TOT.PD.PFT.AMT = Y.TOT.PD.PFT.AMT + Y.PD.PFT.AMT
            Y.PD.PFT = ''
            Y.PD.PFT.AMT = ''
        NEXT I
        
*--------------------------------PD BALANCE-----Y.PD.BAL.TYPE----------------------------------------------------
        Y.TOT.PD.BAL.TYPE.DCOUNT = DCOUNT(Y.PD.BAL.TYPE.ALL,VM)

        FOR I=1 TO Y.TOT.PD.BAL.TYPE.DCOUNT
            Y.PD.BAL.TYPE = Y.PD.BAL.TYPE.ALL<1,I>
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM,Y.PD.BAL.TYPE, Y.TODAY, TOT.PD.BAL.AMT, RetError3)
            Y.PD.AMT = TOT.PD.BAL.AMT
            Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT + Y.PD.AMT
            Y.PD.BAL.TYPE = ''
            Y.PD.AMT = ''
        NEXT I
    
*--------------DUE PRINCIPAL PROFIT (SINGLE DAY ONLY)--------------------

        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.PR.INT, Y.TODAY, DUE.PFT.AMT, RetError3)
        Y.DUE.PFT.AMT = DUE.PFT.AMT

*------------FOR DUE MARKUP PFT -------------------------------------
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.MRK.PR.INT, Y.TODAY, DUE.MRK.PFT.AMT, RetError3)
        Y.DUE.MRK.PFT.AMT = DUE.MRK.PFT.AMT
       
*----------------------------PENALTY PFT ALL-----------------------------

        Y.PENAL.PFT = Y.PENAL.PFT.ALL
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PENAL.PFT, Y.TODAY, TOT.PENAL.PFT.AMT, RetError3)
        Y.PENAL.PFT.AMT = TOT.PENAL.PFT.AMT
        Y.TOT.PENAL.PFT.AMT = Y.TOT.PENAL.PFT.AMT + Y.PENAL.PFT.AMT

*-----------------OVERDUE/BILLS ONLY------------------------------
        Y.OD.PR.AMT.ALL = Y.TOT.PD.BAL.AMT
        Y.OD.PR.AMT.ALL = Y.OD.PR.AMT.ALL * -1
*------------------------------------------------------------------
        Y.CUR.DUE.BAL = Y.CUR.DUE.BAL * -1
        Y.PR.AMT = Y.CUR.BAL + Y.CUR.DUE.BAL
        
        Y.BRANCH = Y.ARR.COM.CODE:'-':Y.COM.NAME
*
*-------------------------------------------------------------------------
        Y.INT.ACCRUALS.ID = Y.ARR.ID:"-":Y.OD.PROPER
        EB.DataAccess.FRead(FN.AA.INT.ACCR, Y.INT.ACCRUALS.ID, REC.INT.DET, F.AA.INT.ACCR, ERR.ACCR)
            
        Y.OD.PR.AMT.ALL = Y.OD.PR.AMT.ALL * -1
*-------------------------------------INSTALLMENT FREQUENCY-------------------------------------------------------
*
        SEL.PAY.SCH = "SELECT ":FN.AA.ARR.PAY.SCH:" WITH @ID LIKE ":Y.ARR.ID:"-":"SCHEDULE-..."
        EB.DataAccess.Readlist(SEL.PAY.SCH, S.LIST.P,"",NO.OF.REC.3,R.CODE)
        Y.P.S.ID = S.LIST.P<NO.OF.REC.3>

        EB.DataAccess.FRead(FN.AA.ARR.PAY.SCH, Y.P.S.ID, P.S.REC, F.AA.ARR.PAY.SCH, PAY.ERR)
        Y.TOT.BILL.TYPE = P.S.REC<AA.PaymentSchedule.PaymentSchedule.PsBillType>
        Y.TOT.PAY.FREQ = P.S.REC<AA.PaymentSchedule.PaymentSchedule.PsPaymentFreq>
        Y.TOT.CALC.AMOUNT = P.S.REC<AA.PaymentSchedule.PaymentSchedule.PsCalcAmount>
              
        CONVERT SM TO VM IN Y.TOT.BILL.TYPE
        CONVERT SM TO VM IN Y.TOT.PAY.FREQ
        CONVERT SM TO VM IN Y.TOT.CALC.AMOUNT
        Y.DCOUNT = DCOUNT(Y.TOT.BILL.TYPE,@VM)
                
        FOR I=1 TO Y.DCOUNT
            Y.BILL.TYPE = Y.TOT.BILL.TYPE<1,I>
            IF Y.BILL.TYPE EQ 'INSTALLMENT' THEN
                Y.PAY.FREQ = Y.TOT.PAY.FREQ<1,I>
*---------------- AA.ARR.PAYMENT.SCHEDULE FOR INS AMT -------------------***
                IF Y.INS.AMT EQ '' THEN
                    Y.INS.AMT = Y.TOT.CALC.AMOUNT<1,I>
                END
            END
        NEXT I

        Y.WEEKLY = Y.PAY.FREQ[10,1]
        Y.MONTHLY = Y.PAY.FREQ[6,1]
        Y.Q.1 = Y.PAY.FREQ[6,1]
        Y.Q = Y.PAY.FREQ[18,1]
        Y.H.YEARLY = Y.PAY.FREQ[6,1]
        Y.YEARLY = Y.PAY.FREQ[2,1]

        IF Y.WEEKLY EQ "1" THEN
            Y.INS.FREQ = "WEEKLY"
        END
        IF Y.MONTHLY EQ "1" THEN
            Y.INS.FREQ = "MONTHLY"
        END
        IF Y.Q.1 EQ "3" OR Y.Q EQ "L" THEN
            Y.INS.FREQ = "QUARTERLY"
        END
    
        IF Y.H.YEARLY EQ "6" THEN
            Y.INS.FREQ = "HALF YEARLY"
        END
        IF Y.YEARLY EQ "1" THEN
            Y.INS.FREQ = "YEARLY"
        END
     
*---------------------------DISBURSEMENT AMOUNT----------------------------------------------
        Y.AA.PROP.CL = 'TERM.AMOUNT'
        PROPERTY.D = 'COMMITMENT'
        AA.Framework.GetArrangementConditions(Y.ARR.ID, Y.AA.PROP.CL, PROPERTY.D, '', RETURN.IDS, RET.VALUES, RET.ERROR4)
        R.REC.TA = RAISE(RET.VALUES)
        Y.DISB.AMT  = R.REC.TA<AA.TermAmount.TermAmount.AmtAmount>

*----------------------------------END----------------------------------------------------
        Y.OUT.DR = Y.PR.AMT + Y.OD.PR.AMT.ALL
        
        IF Y.OUT.DR GT 0 THEN
            Y.OUT.CR = Y.OUT.DR
            Y.OUT.DR = 0
        END
    
        IF Y.OD.INT.RATE EQ '' OR Y.OD.INT.RATE EQ 0 THEN
            Y.OD.INT.RATE = Y.PR.INT.RATE
        END

        IF Y.STATUS EQ '' THEN
            Y.STATUS = "STD"
        END
    
        Y.MARK.DEF.PFT = Y.TOT.PD.PFT.AMT + Y.DUE.PFT.AMT + Y.DUE.MRK.PFT.AMT
    
        Y.COMPENS.AMT = Y.TOT.PENAL.PFT.AMT
        Y.TOT.OUTSTNDNG = Y.PR.AMT + Y.OD.PR.AMT.ALL + Y.MARK.DEF.PFT + Y.PFT.OD + Y.COMPENS.AMT
        
*    Y.ARR.ID = 'AA20019LZH38'
*----------------------SANC DATE FOR ISL---Y.SANC.DATE--Y.ARR.ST.DT--------
        Y.SANC.DATE = Y.ARR.ST.DT

*---------------changed required for BNK ------------------------*
        IF Y.MNEMONIC EQ 'BNK' THEN
            SEL.ACC.CMD = "SELECT ":FN.AA.ARR.ACCT:" WITH @ID LIKE ":Y.ARR.ID:"-":"ACCOUNT-..."
            EB.DataAccess.Readlist(SEL.ACC.CMD, S.LIST.ACC,"",NO.OF.REC.ACC,R.CODE.ACC)
            Y.LN.ACC = S.LIST.ACC<NO.OF.REC.ACC>
        
            EB.DataAccess.FRead(FN.AA.ARR.ACCT, Y.LN.ACC, ACT.REC, F.AA.ARR.ACCT, ACT.ERR)
            Y.SANC.DATE = ACT.REC<AA.Account.Account.AcLocalRef,Y.LT.AC.BD.LNSTDT>
            Y.EXPIRY.DATE = ACT.REC<AA.Account.Account.AcLocalRef,Y.AC.BD.LNMADT.POS>
        END
*-------------------INS AMOUNT FOR HPSM ONLY------------------------
        IF Y.PRD.GRP NE 'IS.MBL.AGR.HPSM.LN' AND Y.PRD.GRP NE 'IS.MBL.COR.HPSM.LN' AND Y.PRD.GRP NE 'IS.MBL.MIC.HPSM.LN' AND Y.PRD.GRP NE 'IS.MBL.RET.HPSM.LN' AND Y.PRD.GRP NE 'IS.MBL.SME.HPSM.LN' THEN
            Y.INS.AMT = ''
        END
    
        IF Y.INS.AMT EQ '' THEN
            Y.INS.AMT = 'NILL'
            Y.INS.FREQ = ''
        END
*-----------------------------------------------------------------------------------------
        IF Y.MNEMONIC EQ 'BNK' THEN
            Y.DATA<-1>= Y.BRANCH:'*':Y.ACC.NUM:'*':Y.CUS.ID:'*':Y.CUS.TITLE:'*':Y.PRD.DES:'*':Y.SANC.DATE:'*':Y.EXPIRY.DATE:'*':Y.PR.INT.RATE:'*':Y.OD.INT.RATE:'*':Y.LIM.AMT:'*':Y.DISB.AMT:'*':Y.INS.AMT:'*':Y.INS.FREQ:'*':Y.CURRENCY:'*':Y.FC.AMT:'*':Y.PR.AMT:'*':Y.OD.PR.AMT.ALL:'*':Y.OUT.DR:'*':Y.OUT.CR:'*':Y.STATUS:'*':Y.SYS.SUS.AMT:'*':Y.MAN.SUS.AMT:'*':Y.MARK.DEF.PFT:'*':Y.PFT.OD:'*':Y.COMPENS.AMT:'*':Y.TOT.OUTSTNDNG
*                       1            2            3             4              5               6                7               8                   9              10              11            12           13              14            15           16            17                18            19           20             21                  22               23              24           25               26
        END
*------------------SORTING BY Y.SANC.DATE ------------------------------------------------
        IF Y.MNEMONIC EQ 'ISL' THEN
            Y.DATA<-1>= Y.SANC.DATE:'*':Y.ACC.NUM:'*':Y.CUS.ID:'*':Y.CUS.TITLE:'*':Y.PRD.DES:'*':Y.BRANCH:'*':Y.EXPIRY.DATE:'*':Y.PR.INT.RATE:'*':Y.OD.INT.RATE:'*':Y.LIM.AMT:'*':Y.DISB.AMT:'*':Y.INS.AMT:'*':Y.INS.FREQ:'*':Y.CURRENCY:'*':Y.FC.AMT:'*':Y.PR.AMT:'*':Y.OD.PR.AMT.ALL:'*':Y.OUT.DR:'*':Y.OUT.CR:'*':Y.STATUS:'*':Y.SYS.SUS.AMT:'*':Y.MAN.SUS.AMT:'*':Y.MARK.DEF.PFT:'*':Y.PFT.OD:'*':Y.COMPENS.AMT:'*':Y.TOT.OUTSTNDNG
*                             1            2            3                4              5            6                7               8                   9              10              11            12           13              14            15           16            17                18            19           20             21                  22               23              24           25               26
        END
           
        Y.SANC.DATE= ''
        Y.EXPIRY.DATE = ''
        
        Y.DISB.AMT = ''
        Y.LIM.AMT = ''
        
        Y.INS.AMT = ''
        Y.PR.AMT = ''
        Y.OD.PR.AMT.ALL = ''
        Y.TOT.PD.BAL.AMT = ''
        Y.OUT.CR = ''
        Y.MARK.DEF.PFT = ''
        Y.PFT.OD = ''
        Y.COMPENS = ''
        Y.TOT.OUTSTNDNG = ''
        Y.TOT.PD.PFT.AMT = ''
        Y.MARK.PFT = ''
        Y.TOT.PENAL.PFT.AMT = ''
        Y.COMPENS.AMT = ''
        Y.DUE.PFT.AMT = ''
        Y.DUE.MRK.PFT.AMT = ''
        
    REPEAT
    
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.DATA = SORT(Y.DATA)
    END
      
RETURN
END