* @ValidationCode : Mjo2NzU3NzQ1MTY6Q3AxMjUyOjE1OTU0MDcyOTE4NTk6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 22 Jul 2020 14:41:31
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


************ID ROUTINE FOR TF HISTORY ID*******************

SUBROUTINE MBL.LC.HIS.COM.ID
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON

*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    
    IF EB.SystemTables.getVFunction() NE 'I'  THEN
        RETURN
    END

    Y.ID.NEW = EB.SystemTables.getIdNew()
    Y.ID.LEN = LEN(Y.ID.NEW)
    Y.ID.INITLEN = Y.ID.NEW[1,2]
    Y.ID.NEW.COLON = Y.ID.NEW[13,1]
    
*****************************************
    IF Y.ID.NEW.COLON NE ';' AND Y.ID.INITLEN NE 'TF' THEN
        EB.SystemTables.setEtext("Please provide a valid History LC ID")
        EB.ErrorProcessing.StoreEndError()
    END
*****************************************************************************
    Y.ID.NEW.1 = Y.ID.NEW[1,12]
    Y.ID.NEW.2 = Y.ID.NEW[14,2]
    Y.ID.FIN = Y.ID.NEW.1:'-':Y.ID.NEW.2
        
    EB.SystemTables.setIdNew(Y.ID.FIN)
RETURN
END