# ammonite-scripts 

this project aims to provide some script utils.

## git log parser
```scala

import $ivy.`com.github.wangzaixiang::ammonite-scrips:1.0.0`;
import scripts.git._
import ammonite.ops._

val dir = root /  "Users"/ "walter01.wang"/ "workspaces"/ "xl" / "api"
val project = GitProject(dir)

val commits = project.log()
  .since(ts"2021-05-06T00:00:00+08:00")
  .until(ts"2021-05-10T00:00:00+08:00")
  .all()
  // .authors("cuiwei")
  .getCommits()
  .filterNot(c => c.messgae.exists( txt => txt.contains("cherry picked from commit")))
  
gitCommitReport(commits)

```

## more