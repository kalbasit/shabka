self: super:

# assert super.gitAndTools.git-appraise == null;

{
  gitAndTools = super.gitAndTools // {
    git-appraise = super.unstable.gitAndTools.git-appraise;
  };
}
