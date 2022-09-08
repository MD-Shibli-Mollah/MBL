SUBROUTINE TF.MBL.V.SALE.CON.COMMO.COD
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.LC.COMMODITY
    $INSERT I_F.BD.SCT.CAPTURE
*-----------------------------------------------------------------------------
    $USING EB.DataAccess
    $USING LC.Contract
    $USING EB.SystemTables
    $USING EB.Updates

    

   
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
INITIALISE:
    
    
RETURN
   
OPENFILE:
    
    FN.COMM.COD='F.BD.LC.COMMODITY'
    F.COMM.COD = ''
   
RETURN
   
PROCESS:
 
    Y.COMMO.COD = EB.SystemTables.getComi()
    Y.COM.NM.TEMP = EB.SystemTables.getRNew(SCT.COMD.DESC)
    Y.COM.NM = Y.COM.NM.TEMP<1,1>
    IF Y.COMMO.COD NE '' AND Y.COM.NM EQ '' THEN
        EB.DataAccess.FRead(FN.COMM.COD, Y.COMMO.COD, REC.COMM, F.COMM.COD, REC.ERR)
        Y.COMM.DES = REC.COMM<BD.DESCRIPTION>
        Y.TEMP = EB.SystemTables.getRNew(SCT.COMD.DESC)
        Y.TEMP<1,1> = Y.COMM.DES
        EB.SystemTables.setRNew(SCT.COMD.DESC, Y.TEMP)
    END
     
       
      
RETURN
END
