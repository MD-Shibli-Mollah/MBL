* @ValidationCode : MjoyMDA4OTcwOTY2OkNwMTI1MjoxNjA0Mjk5NDc0NDI5OkRFTEw6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 02 Nov 2020 12:44:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : DEV_201710.0
*-----------------------------------------------------------------------------
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE BD.NPA.STATUS.DETAILS.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine BDMB.NPA.STATUS.DETAILS.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    
    $USING EB.SystemTables
    $USING ST.AssetProcessing
    
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId('NPA.DET.ID', T24_String) ;* Define Table id
    EB.SystemTables.setIdCheckfile('AA.ARRANGEMENT')
*-----------------------------------------------------------------------------
    CALL Table.addFieldDefinition('XX<ASSET.CLASS','3','A','')
    CALL Field.setCheckFile('LN.ASSET.CLASS':FM:ST.AssetProcessing.LnAssetClass.LnAssclsShortDesc:FM:'L')
    CALL Table.addFieldDefinition('XX-START.DATE','12','D','')
    CALL Table.addFieldDefinition('XX>CALC.OD.DAYS','5','A','')
    CALL Table.addFieldDefinition('LAST.ASSET.CLASS','3','A','')
    CALL Field.setCheckFile('LN.ASSET.CLASS':FM:ST.AssetProcessing.LnAssetClass.LnAssclsShortDesc:FM:'L')
    CALL Table.addFieldDefinition('GEN.TYPE', '8', '': FM : 'SYSTEM_USER','')
    CALL Table.addField('PRODUCT.GROUP', T24_String, '', '')
    CALL Table.addFieldDefinition('MAINTAIN_MANUAL', '3', '': FM : 'YES_','')
*-----------------------------------------------------------------------------
    CALL Table.addFieldDefinition('XX<PROV.DATE','12','D','')
    CALL Table.addFieldDefinition('XX-PROV.CALC.AMT','21','AMT','')
    CALL Table.addFieldDefinition('XX-PROV.ENT.AMT','21','AMT','')
    CALL Table.addReservedField('RESERVED.12')
    CALL Table.addFieldDefinition('XX>PROV.BALANCE','21','AMT','')
    CALL Table.addFieldDefinition('NET.BALANCE','21','AMT','')
    CALL Table.addReservedField('RESERVED.11')
    CALL Table.addReservedField('RESERVED.10')
    CALL Table.addReservedField('RESERVED.09')
    CALL Table.addReservedField('RESERVED.08')
    CALL Table.addReservedField('RESERVED.07')
    CALL Table.addReservedField('RESERVED.06')
    CALL Table.addReservedField('RESERVED.05')
    CALL Table.addReservedField('RESERVED.04')
    CALL Table.addReservedField('RESERVED.03')
    CALL Table.addReservedField('RESERVED.02')
    CALL Table.addReservedField('RESERVED.01')
    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
