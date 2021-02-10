package com.raphtory.examples.oag

/*case class(
"label_density": -1,
"num_blockchain_name": -1,
"num_chargingandrewardingsystem": -1,
"num_codebase": -1,
"num_consensus": -1,
"num_esg": -1,
"num_extensibility": -1,
"num_identifiers": -1,
"num_identity_management": -1,
"num_labelled_token": -1,
"num_misc": -1,
"num_native_currency_tokenisation": -1,
"num_security_privacy": -1,
"num_token": -1,
"num_transaction_capabilities": -1,
)*/

case class OpenAcademic(
/*"title": "1 Blockchain's roles in meeting key supply chain management objectives",
"authors.name": ["N. Kshetri"],
"year": 2018,
"Corpus_ID": "46783030",
"doi": "10.1016/j.ijinfomgt.2017.12.005",
"fos": "Business, Computer Science",
"publisher":"Int. J. Inf. Manag.",
"pdf": "http://libres.uncg.edu/ir/uncg/f/N_Kshetri_Blockchains_Roles_2018.pdf",
"bibtex_citation": "@article{Kshetri20181BR,
        title={1 Blockchain's roles in meeting key supply chain management objectives},
author={N. Kshetri},
journal={Int. J. Inf. Manag.},
year={2018},
volume={39},
pages={80-89}
}",
    "references":[{
*/


  //naming fields
  title: Option[String],
  doi: Option[String],
  paperId: Option[String],
  year: Option[Int],
  references: Option[List[OpenAcademic]],
  citations: Option[List[OpenAcademic]],
  isSeed: Option[Boolean],
  labelDensity: Option[Double],
  s3_key: Option[String],
  num_blockchain_name: Option[Double],
  num_chargingandrewardingsystem: Option[Double],
  num_codebase: Option[Double],
  num_consensus: Option[Double],
  num_esg: Option[Double],
  num_extensibility: Option[Double],
  num_identifiers: Option[Double],
  num_identity_management: Option[Double],
  num_labelled_token: Option[Double],
  num_misc: Option[Double],
  num_native_currency_tokenisation: Option[Double],
  num_security_privacy: Option[Double],
  num_token: Option[Double],
  num_transaction_capabilities: Option[Double],
)
