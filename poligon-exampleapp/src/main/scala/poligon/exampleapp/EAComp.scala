package poligon.exampleapp

import poligon.comp.Comps
import poligon.exampleapp.services.Services
import poligon.tran.LanguageComps

object EAComp extends Comps[Services] with LanguageComps[Services]
