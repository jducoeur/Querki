package models

import querki.test._

class OIDTests extends QuerkiTests {
  "IndexedOID" should {
    "handle an indexed ID" in {
      IndexedOID.parse("abc[123]") should equal(Some(IndexedOID(OID("abc"), Some(123))))
    }
    
    "handle an unindexed ID" in {
      IndexedOID.parse("abc") should equal(Some(IndexedOID(OID("abc"), None)))      
    }
    
    "handle a bogus input" in {
      IndexedOID.parse("floob!") should equal(None)
    }
  }
}