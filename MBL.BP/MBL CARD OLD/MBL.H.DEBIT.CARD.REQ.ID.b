SUBROUTINE MBL.H.DEBIT.CARD.REQ.ID
*-----------------------------------------------------------------------------
    !** FIELD definitions FOR MBL.H.DEBIT.CARD.REQ
*!
* @author youremail@temenos.com
* @stereotype id
* @package infra.eb
* @uses E
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
*-----------------------------------------------------------------------------
* TODO Add logic to validate the id
* TODO Create an EB.ERROR record if you are creating a new error code
*-----------------------------------------------------------------------------
    Y.ID = EB.SystemTables.getComi()
   
    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.FRead(FN.AC,Y.ID,REC.AC,F.AC, Y.ERR)
    Y.AC.CO.CODE = REC.AC<AC.AccountOpening.Account.CoCode>
      
    IF Y.AC.CO.CODE NE EB.SystemTables.getIdCompany() THEN
        EB.SystemTables.setE('OTHER BRANCH AC')
        RETURN
    END
RETURN
END
