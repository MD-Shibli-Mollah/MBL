* @ValidationCode : MjotMTc4NjY4MTg4NTpDcDEyNTI6MTYzMjE0OTA2NDA0NTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 20 Sep 2021 20:44:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.E.CNV.USER.NAME(Y.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    
    $USING EB.Reports
    $USING EB.DatInterface
    $USING EB.DataAccess
    $USING EB.SystemTables
    
    Y.USER = EB.SystemTables.getRUser()
    
* EB.Reports.setOData(Y.USER)
    Y.DATA = Y.USER
    
END
