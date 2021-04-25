import {bar} from "./diamond_dependency_2.kk"
import {spam} from "./diamond_dependency_3.kk"

do [null.bar(), null.spam()].print()