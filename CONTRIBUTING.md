# Contributing to Aura

Thank you for your interest in contributing to Aura. This document provides guidelines for contributions.

## Contributor License Agreement

This project welcomes contributions and suggestions. Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

The `license/cla` check is provided by the Microsoft GitHub Policy Service.
Follow that check's instructions; this repository does not maintain a separate
CLA signature file or ask contributors to sign an additional agreement.

## Code of Conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Getting Started

1. Fork the repository and clone your fork.
2. Install JDK 17+ and sbt 2.0+.
3. Build and run tests:
   ```bash
   sbt compile
   sbt test
   ```

## Making Changes

1. Open an issue first to discuss the proposed change.
2. Create a branch from `main` for your changes.
3. Write tests for new functionality.
4. Ensure all tests pass: `sbt test`
5. Format code: `sbt scalafmtAll`
6. Keep commits focused and write clear commit messages.

## Code Style

- All state must be immutable. Actor state transitions produce new values.
- Policies are pure functions (type aliases, not class hierarchies).
- Use opaque types for domain quantities (`MIPS`, `SimTime`, `MegaBytes`, etc.).
- Follow existing module structure: `actors/`, `state/`, `policies/`.

## Pull Requests

1. Reference the discussion issue in your PR description.
2. Keep PRs small and focused on a single concern.
3. Ensure CI passes before requesting review.

## Reporting Issues

When reporting a bug, include:
- Scala and JDK version
- Minimal simulation configuration that reproduces the issue
- Expected vs. actual behavior
- Stack trace (if applicable)

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
