* @ValidationCode : MjotMjgyODgxNDE6Q3AxMjUyOjE2MjczNjc4Njg2OTc6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Jul 2021 12:37:48
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

SUBROUTINE CR.MBL.ENQ.BR.BLD.STAFFLN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.ST.PRS.P.LN MBL.CAR.PRN.LN MBL.EHBL.PRN.LN MBL.PRV.FND.LN'
 
RETURN
END