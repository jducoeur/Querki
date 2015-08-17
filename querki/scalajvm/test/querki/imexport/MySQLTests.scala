package querki.imexport

import mysql._

import fastparse.all._

import models._
import models.Thing._

import querki.ecology._
import querki.test._
import querki.time._
import querki.util.QLog

/**
 * @author jducoeur
 */
class MySQLTests extends QuerkiTests with ParserTests {
  import MySQLParse._
  import MySQLProcess._
  
  "MySQLParse" should {
    lazy val Core = interface[querki.core.Core]
    
    "parse end of line" in {
      checkParse(MySQLParse.nlP, "\n")
      checkParse(MySQLParse.nlP, "\r\n")
    }
    
    "parse a couple of blank lines" in {
      checkParse(MySQLParse.blankCommentP, """
""")
      checkParse(MySQLParse.blankCommentP, """   
""")
      checkParse(MySQLParse.commentsP, """
   
""")
    }
    
    "parse a hash comment" in {
      checkParse(MySQLParse.hashCommentP, """# ************************************************************
""")
      checkParse(MySQLParse.hashCommentP, "# ************************************************************\r\n")
      checkParse(MySQLParse.commentsP, """
# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
""")
    }
    
    "parse a quoted identifier" in {
      checkParse(MySQLParse.quotedIdentP, "`id`")
    }
    
    "parse a create statement" in {
      checkParse(MySQLParse.createStatementP, """CREATE TABLE `case` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `last_updated` timestamp NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP
)
""")
      checkParse(MySQLParse.statementP, """CREATE TABLE `case` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT
);
""")
    }
    
    "parse a quoted value" in {
      val s = checkParse(MySQLParse.oneValueP, "'2012-06-13'")
      assert(s == "2012-06-13")
    }
    
    "parse values in a simple insert row" in {
      val row:RawRow = checkParse(MySQLParse.rowValuesP, "('2012-06-13','2012-07-13 10:28:37')")
      assert(row.v(0) == "2012-06-13")
    }
    
    "parse an insert statement" in {
      checkParse(MySQLParse.insertStatementP, """INSERT INTO `movement` (`id`, `brand`, `serial_no`, `size_mm`, `size`, `jewel_count`, `wind_type_id`, `set_type_id`, `escapement_type`, `dial_photo`, `mvmt_photo`, `source_id`, `price`, `lot_number`, `date_purchased`, `last_updated`, `notes`)
VALUES
  (1,'Howard',290,NULL,'N',15,5,4,NULL,0,0,1,132.5,90,'2011-06-16','2011-03-13 10:28:01',NULL),
  (5,'US Watch Company',65,NULL,'16',7,5,4,NULL,1,1,2,30.5,27,'2011-06-12','2012-03-20 15:15:01',NULL)""")
    }
    
    "read in a bit of dumpfile" in {
      // This is not intended to be a valid dumpfile -- it just exercises a bunch of syntax:
      val statements = MySQLParse("""
# ************************************************************

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;

DROP TABLE IF EXISTS `case`;

CREATE TABLE `case` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `last_updated` timestamp NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
);

INSERT INTO `movement` (`id`, `brand`, `serial_no`, `size_mm`, `size`, `jewel_count`, `wind_type_id`, `set_type_id`, `escapement_type`, `dial_photo`, `mvmt_photo`, `source_id`, `price`, `lot_number`, `date_purchased`, `last_updated`, `notes`)
VALUES
  (1,'Howard',290,NULL,'N',15,5,4,NULL,0,0,1,132.5,90,'2011-06-16','2011-03-13 10:28:01',NULL),
  (5,'US Watch Company',65,NULL,'16',7,5,4,NULL,1,1,2,30.5,27,'2011-06-12','2012-03-20 15:15:01',NULL);
""")
      assert(statements.size == 3)
      statements.head match {
        case StmtDrop(TableName(table)) => assert(table == "case")
        case _ => fail("Didn't get the expected drop statement!")
      }
    }
    
    "read in a full dumpfile" in {
      val statements = MySQLParse(sql)
      
      assert(statements.size == 26)
    }
  }
  
  implicit class RichTable(table:MySQLTable) {
    def rows = table.data.get.rows
    def rowOpt(id:Int):Option[MySQLRow] = rows.find(_.vs.head == IntVal(id))
    
    def columnIndex(colName:String):Int = 
      table.data.get.columnOrder.indexWhere(_.v == colName)
    
    def cell(rowId:Int, colName:String):SQLVal[_] = {
      val r = rowOpt(rowId).get
      r.vs(columnIndex(colName))
    }
  }
  
  "MySQLProcess" should {
    "successfully build a complex DB" in {
      val statements = MySQLParse(sql)
    
      val db = MySQLProcess.processStmts(statements)

      // Some spot-checks of the resulting Tables:
      val movements = db.tables(TableName("movement"))
      assert(movements.rows.size == 17)
      val record17 = movements.rowOpt(17)
      assert(record17.isDefined)
      assert(record17.get.vs.tail.head == VarcharVal("Howard"))
      assert(movements.cell(9, "brand") == VarcharVal("Waltham"))
      assert(movements.cell(19, "size_mm") == NullVal)
      assert(movements.cell(13, "date_purchased") == DateVal(new DateTime(2012, 1, 1, 0, 0)))
      assert(movements.primaryKey == Some(ColumnName("id")))
    }
  }
  
  "MySQLImport" should {
    // Stripped-down test, just to check that constraints become Links:
    "construct a constraint correctly" in {
      val importer = new MySQLImport(SimpleTestRequestContext(BasicTestUser.mainIdentity.id), "Watches Space")(ecology)
      val state = importer.readDumpfile("""
CREATE TABLE `movement` (
  `set_type_id` int(11) unsigned DEFAULT NULL,
  CONSTRAINT `movement_set_rel` FOREIGN KEY (`set_type_id`) REFERENCES `movement_set_type` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
);

CREATE TABLE `movement_set_type` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `source` varchar(64) DEFAULT NULL,
  PRIMARY KEY (`id`)
);
""")

      val mstModel = state.anythingByDisplayName("Movement Set Type").get
      val sourceProp = state.prop(AsName("Source")).get.confirmType(Core.TextType).get
    }
    
    // This is the serious test -- do we get the right output?
    "produce a correct Space from a complex DB" in {
      val importer = new MySQLImport(SimpleTestRequestContext(BasicTestUser.mainIdentity.id), "Watches Space")(ecology)
      val state = importer.readDumpfile(sql)
      
      def pqloaded(text:String) = {
        val rc = getRcs(state)(commonSpace, BasicTestUser)
        val context = state.thisAsContext(rc, state, ecology)
        processQText(context, text)
      }
      
      pqloaded("[[Movement Wind Type._instances -> _sort(Wind Type Name) -> Wind Type Name -> _commas]]") should
        startWith("Key Back, Key Front")
    }
  }
  
  val sql = """
# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: 127.0.0.1 (MySQL 5.1.73-log)
# Database: watches
# Generation Time: 2015-04-09 17:59:18 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table case
# ------------------------------------------------------------

DROP TABLE IF EXISTS `case`;

CREATE TABLE `case` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `case_type_id` int(11) unsigned DEFAULT NULL,
  `case_material_id` int(11) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `case_type_id` (`case_type_id`),
  KEY `case_material_id` (`case_material_id`),
  CONSTRAINT `case_material_id` FOREIGN KEY (`case_material_id`) REFERENCES `case_material` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `case_type_id` FOREIGN KEY (`case_type_id`) REFERENCES `case_type` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table case_material
# ------------------------------------------------------------

DROP TABLE IF EXISTS `case_material`;

CREATE TABLE `case_material` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `case_material_name` varchar(64) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table case_type
# ------------------------------------------------------------

DROP TABLE IF EXISTS `case_type`;

CREATE TABLE `case_type` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `case_type_name` varchar(64) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table movement
# ------------------------------------------------------------

DROP TABLE IF EXISTS `movement`;

CREATE TABLE `movement` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `brand` varchar(64) NOT NULL DEFAULT '',
  `serial_no` int(20) DEFAULT NULL,
  `size_mm` double DEFAULT NULL,
  `size` varchar(8) DEFAULT NULL,
  `jewel_count` int(11) DEFAULT NULL,
  `wind_type_id` int(11) unsigned DEFAULT NULL,
  `set_type_id` int(11) unsigned DEFAULT NULL,
  `escapement_type` int(11) DEFAULT NULL,
  `dial_photo` int(1) NOT NULL DEFAULT '0',
  `mvmt_photo` int(1) NOT NULL DEFAULT '0',
  `source_id` int(11) unsigned DEFAULT NULL,
  `price` double DEFAULT NULL,
  `lot_number` bigint(18) DEFAULT NULL,
  `date_purchased` date DEFAULT NULL,
  `last_updated` timestamp NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  `notes` longtext,
  PRIMARY KEY (`id`),
  KEY `wind_type_id_idx` (`wind_type_id`),
  KEY `set_type_id_idx` (`set_type_id`),
  KEY `movement_source_rel` (`source_id`),
  CONSTRAINT `movement_set_rel` FOREIGN KEY (`set_type_id`) REFERENCES `movement_set_type` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `movement_source_rel` FOREIGN KEY (`source_id`) REFERENCES `movement_source` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `movement_wind_rel` FOREIGN KEY (`wind_type_id`) REFERENCES `movement_wind_type` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `movement` WRITE;
/*!40000 ALTER TABLE `movement` DISABLE KEYS */;

INSERT INTO `movement` (`id`, `brand`, `serial_no`, `size_mm`, `size`, `jewel_count`, `wind_type_id`, `set_type_id`, `escapement_type`, `dial_photo`, `mvmt_photo`, `source_id`, `price`, `lot_number`, `date_purchased`, `last_updated`, `notes`)
VALUES
  (1,'Howard',207090,NULL,'N',15,5,4,NULL,0,0,1,132.5,23720090,'2012-01-01','2012-07-13 10:28:37',NULL),
  (5,'US Watch Company',704765,NULL,'16',7,5,4,NULL,1,1,2,30.5,2210420677,'2012-01-11','2012-07-22 15:15:18',NULL),
  (6,'Elgin',349332,NULL,'10',15,2,2,NULL,0,0,3,20,20925386729,'2012-07-02','2012-01-11 10:33:09',NULL),
  (7,'Anonymous',200704,NULL,NULL,17,5,6,NULL,1,1,4,16.66,NULL,'2012-07-12','2012-01-21 15:18:11','Part of a lot of 6 movements at $140.  Probably German'),
  (8,'Buren',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,5,41.36,2710171983,NULL,'2012-01-21 23:41:06',NULL),
  (9,'Waltham',1717552,NULL,NULL,11,5,4,NULL,0,0,6,20,281703146,NULL,'2012-01-01 19:34:54','part of a lot of 2 movements @41.77'),
  (10,'Waltham',14061271,NULL,'16',17,5,6,NULL,0,0,6,22,221703146,NULL,'2012-01-01 19:37:24','part of a lot of 2 movements @41.77'),
  (11,'Illinois',529474,NULL,'10',7,4,4,NULL,0,0,7,19.23,837658142,'2012-01-21','2012-09-09 14:48:33','dial damaged'),
  (12,'Elgin',1196771,NULL,'10',7,2,2,NULL,0,0,8,23.69,230836912,'2012-01-11','2012-09-09 14:51:27','dial pins missing'),
  (13,'Waltham',718154,NULL,'10',15,2,2,NULL,0,0,9,50,1907244014,'2012-01-01','2012-09-09 14:54:07',NULL),
  (14,'Howard',NULL,79,NULL,13,1,NULL,NULL,0,0,10,85.95,2163813548,'2012-01-01','2012-10-27 22:49:59',NULL),
  (15,'Movado',NULL,38,NULL,15,4,6,NULL,0,0,11,47.64,2907868599,'2012-01-01','2012-10-31 15:48:21','no case but spacer ring.'),
  (16,'Camden, London',72084,NULL,NULL,0,2,1,NULL,0,0,12,20.7,3944706934,'2013-01-01','2013-02-28 23:05:12',NULL),
  (17,'Howard',52763,NULL,'L',13,4,2,NULL,0,0,13,420,3905473284,'2013-03-04','2013-01-01 11:48:06',NULL),
  (18,'US Watch Company',18937,NULL,'16',15,NULL,NULL,NULL,0,0,14,10.38,3798748307,'2013-01-01','2013-05-31 18:53:37',NULL),
  (19,'Philidephia',3226,NULL,NULL,NULL,NULL,NULL,NULL,0,0,15,96.88,2612189576,'2013-01-01','2013-05-31 18:53:25',NULL),
  (20,'Illinois',437835,NULL,'8',NULL,NULL,NULL,NULL,0,0,16,10.5,1610941100,'2013-01-01','2013-07-26 19:22:46',NULL);

/*!40000 ALTER TABLE `movement` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table movement_set_type
# ------------------------------------------------------------

DROP TABLE IF EXISTS `movement_set_type`;

CREATE TABLE `movement_set_type` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `set_type_name` varchar(63) DEFAULT NULL,
  `enabled` int(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `movement_set_type` WRITE;
/*!40000 ALTER TABLE `movement_set_type` DISABLE KEYS */;

INSERT INTO `movement_set_type` (`id`, `set_type_name`, `enabled`)
VALUES
  (1,'Key Front',1),
  (2,'Key Back',1),
  (3,'Transitional',1),
  (4,'Lever',1),
  (5,'Pin',1),
  (6,'Crown',1),
  (7,'Other',1);

/*!40000 ALTER TABLE `movement_set_type` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table movement_source
# ------------------------------------------------------------

DROP TABLE IF EXISTS `movement_source`;

CREATE TABLE `movement_source` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `source` varchar(64) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `movement_source` WRITE;
/*!40000 ALTER TABLE `movement_source` DISABLE KEYS */;

INSERT INTO `movement_source` (`id`, `source`)
VALUES
  (1,'eBay - ss'),
  (2,'eBay - jb'),
  (3,'eBay - bg2'),
  (4,'Brimfield - v'),
  (5,'eBay - be'),
  (6,'eBay - ttj'),
  (7,'eBay - bl'),
  (8,'eBay - pw'),
  (9,'eBay - ed'),
  (10,'eBay - hn'),
  (11,'eBay - wp'),
  (12,'eBay - lw'),
  (13,'eBay - ot'),
  (14,'eBay - ft'),
  (15,'eBay - wb'),
  (16,'eBay - wo ');

/*!40000 ALTER TABLE `movement_source` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table movement_wind_type
# ------------------------------------------------------------

DROP TABLE IF EXISTS `movement_wind_type`;

CREATE TABLE `movement_wind_type` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `wind_type_name` varchar(63) DEFAULT NULL,
  `enabled` int(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `movement_wind_type` WRITE;
/*!40000 ALTER TABLE `movement_wind_type` DISABLE KEYS */;

INSERT INTO `movement_wind_type` (`id`, `wind_type_name`, `enabled`)
VALUES
  (1,'Key Front',1),
  (2,'Key Back',1),
  (3,'Transitional',1),
  (4,'Stem (12 o\'clock)',1),
  (5,'Stem (3 o\'clock)',1),
  (6,'Other',1);

/*!40000 ALTER TABLE `movement_wind_type` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
"""
}