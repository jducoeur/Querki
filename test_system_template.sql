-- MySQL dump 10.13  Distrib 5.5.47, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: test_system_template
-- ------------------------------------------------------
-- Server version	5.5.47-0ubuntu0.14.04.1-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `Apps`
--

DROP TABLE IF EXISTS `Apps`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Apps` (
  `space_id` bigint(20) NOT NULL,
  `app_id` bigint(20) NOT NULL,
  `app_version` bigint(20) DEFAULT '0',
  `position` int(11) DEFAULT '0',
  PRIMARY KEY (`space_id`,`app_id`),
  KEY `spaceKey` (`space_id`),
  KEY `appKey` (`app_id`),
  CONSTRAINT `space_rel` FOREIGN KEY (`space_id`) REFERENCES `Spaces` (`id`) ON DELETE CASCADE,
  CONSTRAINT `app_rel` FOREIGN KEY (`app_id`) REFERENCES `Spaces` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Apps`
--

LOCK TABLES `Apps` WRITE;
/*!40000 ALTER TABLE `Apps` DISABLE KEYS */;
/*!40000 ALTER TABLE `Apps` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Identity`
--

DROP TABLE IF EXISTS `Identity`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Identity` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) NOT NULL,
  `userId` bigint(20) DEFAULT NULL,
  `kind` int(11) NOT NULL,
  `provider` int(11) DEFAULT NULL,
  `handle` varchar(255) DEFAULT NULL,
  `email` varchar(255) DEFAULT NULL,
  `authentication` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `identity_by_email` (`email`),
  KEY `identity_by_name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Identity`
--

LOCK TABLES `Identity` WRITE;
/*!40000 ALTER TABLE `Identity` DISABLE KEYS */;
INSERT INTO `Identity` VALUES (98,'Test User 1',11,2,NULL,'testadmin1','test1@querki.net','5q3b7hkpbsxfh.1dn1vd31ft8a0jbrebofkp67ijkobc1u');
/*!40000 ALTER TABLE `Identity` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `OIDNexter`
--

DROP TABLE IF EXISTS `OIDNexter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `OIDNexter` (
  `nextId` int(11) NOT NULL,
  `shard` int(11) DEFAULT '1'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `OIDNexter`
--

LOCK TABLES `OIDNexter` WRITE;
/*!40000 ALTER TABLE `OIDNexter` DISABLE KEYS */;
INSERT INTO `OIDNexter` VALUES (0,1);
/*!40000 ALTER TABLE `OIDNexter` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SpaceMembership`
--

DROP TABLE IF EXISTS `SpaceMembership`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SpaceMembership` (
  `identityId` bigint(20) NOT NULL,
  `spaceId` bigint(20) NOT NULL,
  `membershipState` tinyint(4) DEFAULT '0',
  `lastAccess` datetime DEFAULT NULL,
  `nickname` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`identityId`,`spaceId`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SpaceMembership`
--

LOCK TABLES `SpaceMembership` WRITE;
/*!40000 ALTER TABLE `SpaceMembership` DISABLE KEYS */;
/*!40000 ALTER TABLE `SpaceMembership` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Spaces`
--

DROP TABLE IF EXISTS `Spaces`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Spaces` (
  `id` bigint(20) NOT NULL,
  `shard` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `display` varchar(255) NOT NULL,
  `owner` bigint(20) NOT NULL,
  `size` int(11) NOT NULL,
  `version` int(11) DEFAULT '1',
  `status` int(11) DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `spaces_by_owner` (`owner`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Spaces`
--

LOCK TABLES `Spaces` WRITE;
/*!40000 ALTER TABLE `Spaces` DISABLE KEYS */;
/*!40000 ALTER TABLE `Spaces` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `User`
--

DROP TABLE IF EXISTS `User`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `User` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `level` tinyint(4) DEFAULT NULL,
  `join_date` datetime DEFAULT NULL,
  `tosVersion` int(11) DEFAULT '0',
  `userVersion` int(11) DEFAULT '0',
  `lastNoteChecked` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `User`
--

LOCK TABLES `User` WRITE;
/*!40000 ALTER TABLE `User` DISABLE KEYS */;
INSERT INTO `User` VALUES (11,'testadmin1',10,NULL,0,0,0);
/*!40000 ALTER TABLE `User` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2016-01-27 17:40:20
