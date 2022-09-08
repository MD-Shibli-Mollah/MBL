* @ValidationCode : Mjo0OTM4OTkwOTg6Q3AxMjUyOjE2MzE1MjU5ODg3NDY6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 13 Sep 2021 15:39:48
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

SUBROUTINE CR.MBL.ENQ.BR.BLD.DEMANDLN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.FACT.RECEI.LN MBL.FACT.RECEI.NRV.LN MBL.FORCE.BG.LN MBL.FORCE.IMP.LN MBL.FO.LN MBL.IBP.CLN MBL.SHRT.TRM.LN MBL.SOD.WO.LN MBL.TIME.FIS.LN MBL.TIME.FRC.LN MBL.TIME.LN'
 
RETURN
END