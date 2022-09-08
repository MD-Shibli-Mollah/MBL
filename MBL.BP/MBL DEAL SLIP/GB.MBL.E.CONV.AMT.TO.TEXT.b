* @ValidationCode : MjotMjU1OTA2NDM0OkNwMTI1MjoxNjMyMTQ5MTc4NDE4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 20 Sep 2021 20:46:18
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.E.CONV.AMT.TO.TEXT(Y.DATA)
*=============================================================================
* Subroutine type    : Subroutine
* Attached to        : MBL.DS.FT.LCY
* Primary Purpose    : This Routine used to Converting Amount in words.
* Created by         : Md Rayhan Uddin
* Change History     : Manjunath Suvarna
*                    : Sudheep V
*=============================================================================

*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*  $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.Reports
    $USING EB.Foundation
    $USING TT.Contract
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
       
RETURN

*****
INIT:
*****
   

**********
OPENFILES:
**********
       
    
    
RETURN

********
PROCESS:
********
 
* LNGVAR = EB.Reports.getOData()
    LNGVAR = Y.DATA
    TXTOUT = ''
    TXTVAR1=''
    INTVAL=''
    Y.COMI.LEN = LEN(LNGVAR)
    IF Y.COMI.LEN LT 20 THEN
        
        INTVAL = FIELD(LNGVAR,'.',1)
        INTVAL3 = FIELD(LNGVAR,'.',2)

        IF INTVAL3 NE 0 THEN
            INTVAL2=INTVAL3
        END ELSE
            INTVAL2=0
        END

        CORE=INT(INTVAL / 10000000)
        CALL CM.CALHUND(CORE,INTCORE)
        INTVAL = INT(INTVAL - INT(INTVAL / 10000000) * 10000000)

        LAC=INT(INTVAL / 100000)
        CALL CM.CALHUND(LAC,INTLAC)
        INTVAL = INT(INTVAL - INT(INTVAL / 100000) * 100000)

        THOUSAND=INT(INTVAL / 1000)
        CALL CM.CALHUND(THOUSAND,INTTHOUSAND)
        INTVAL = INT(INTVAL - INT(INTVAL / 1000) * 1000)

        HUNDRED=INT(INTVAL / 100)
        CALL CM.CALHUND(HUNDRED,INTHUNDRED)
        INTVAL = INT(INTVAL - INT(INTVAL / 100) * 100)

        REST=INT(INTVAL / 1)
        CALL CM.CALHUND(REST,INTREST)

        DES=INT(INTVAL2 / 1)
        CALL CM.CALHUND(DES,INTDES)

        IF LEN(INTCORE) EQ 0 THEN
            TXTVAR1=INTCORE:" ":""
        END ELSE
            TXTVAR1=INTCORE:" ":"Crore"
        END

        IF LEN(INTLAC) EQ 0 THEN
            TXTVAR1=TXTVAR1:" ":INTLAC:"":""
        END ELSE
            TXTVAR1=TXTVAR1:" ":INTLAC:" ":"Lac"
        END

        IF LEN(INTTHOUSAND) EQ 0 THEN
            TXTVAR1=TXTVAR1:" ":INTTHOUSAND:"":""
        END ELSE
            TXTVAR1=TXTVAR1:" ":INTTHOUSAND:" ":"Thousand"
        END

        IF LEN(INTHUNDRED) EQ 0 THEN
            TXTVAR1=TXTVAR1:" ":INTHUNDRED:"":""
        END ELSE
            TXTVAR1=TXTVAR1:" ":INTHUNDRED:" ":"Hundred"
        END

        TXTVAR1=TXTVAR1:" ":INTREST:" ":"Taka"

        IF LEN(INTDES) EQ 0 THEN
        END ELSE
            TXTVAR1=TXTVAR1:" ":"and":" ":INTDES:" ":"Paisa"
        END
    
        TXTOUT = TXTVAR1
    
* EB.Reports.setOData(TXTOUT)
        Y.DATA = TXTOUT
    
    END

RETURN

END