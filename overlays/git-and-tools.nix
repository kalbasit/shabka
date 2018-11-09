self: super:

{
  gitAndTools = assert ! super.gitAndTools ? git-appraise; super.gitAndTools // {
    git-appraise = super.unstable.gitAndTools.git-appraise;
  };
}
