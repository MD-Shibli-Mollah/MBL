* @ValidationCode : MjoxNDI4ODUwNzI2OkNwMTI1MjoxNjI5NTQ0NDY1NDg0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Aug 2021 17:14:25
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

SUBROUTINE CR.MBL.ENQ.BLD.RETAIL.UNAU(ENQ.DATA)
    
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.SOD.FDR.RET.LN MBL.SOD.SCH.DP.RET.LN MBL.LOAN.FO.RET.LN MBL.PROV.FND.RET.LN MBL.HBL.RES.RET.LN MBL.HOME.RET.LN MBL.HOME.REF.RET.LN MBL.PERS.RET.LN MBL.AUTO.RET.LN MBL.EDU.RET.LN MBL.HOUS.FUR.RET.LN MBL.OVRS.EMPL.RET.LN MBL.DOC.RET.LN MBL.ANY.PUR.RET.LN MBL.COTT.RET.LN MBL.PRANTIK.RET.LN MBL.OTH.CONSUMER.LN MBL.SML.SCH.LN'

RETURN
END