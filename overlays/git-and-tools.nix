self: super:

{
  # TODO: how to check for overlay assertions per host?
  gitAndTools = /* assert ! super.gitAndTools ? git-appraise; */ super.gitAndTools // {
    git-appraise = super.unstable.gitAndTools.git-appraise;
  };
}
