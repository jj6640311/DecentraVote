# DecentraVote
 
## Overview

DecentraVote is a decentralized voting platform built on the Stacks blockchain using Clarity smart contracts. It provides a secure, transparent, and efficient way for DAOs and organizations to conduct token-weighted governance.

## Features

- **Token-weighted Voting**: Votes are weighted based on the number of tokens staked by participants
- **Proposal Management**: Create, track, and execute proposals
- **Stake-based Participation**: Users stake tokens to participate in governance
- **Transparent Results**: All voting results are publicly verifiable on-chain
- **Automatic Execution**: Proposals can be automatically executed once voting concludes

## Smart Contract Architecture

The core of DecentraVote is a Clarity smart contract that manages the entire voting process:

### Key Components

- **Proposal System**: Create and manage proposals with customizable parameters
- **Staking Mechanism**: Users stake tokens to gain voting power
- **Voting Logic**: Secure voting with protection against double-voting
- **Result Calculation**: Automatic tallying of votes and determination of outcomes

## Usage

### For Organizations

1. **Create a Proposal**:
   - Define a proposal with title, description, and voting duration
   - Set minimum token requirements for proposal creation

2. **Monitor Voting**:
   - Track participation and vote distribution in real-time
   - View detailed analytics on voter engagement

3. **Execute Proposals**:
   - Automatically implement successful proposals
   - Maintain a transparent record of all governance decisions

### For Voters

1. **Stake Tokens**:
   - Stake tokens to participate in governance
   - Voting power is proportional to staked amount

2. **Vote on Proposals**:
   - Cast votes on active proposals
   - Each token represents voting weight

3. **Unstake Tokens**:
   - Retrieve tokens when not actively voting
   - Maintain flexibility in token management

## Technical Details

### Smart Contract Functions

#### Proposal Management
- `create-proposal`: Create a new governance proposal
- `execute-proposal`: Execute a proposal after voting concludes
- `get-proposal`: Retrieve details about a specific proposal
- `get-proposal-count`: Get the total number of proposals

#### Token Staking
- `stake-tokens`: Stake tokens to gain voting rights
- `unstake-tokens`: Withdraw staked tokens
- `get-user-stake`: Check a user's current stake

#### Voting
- `vote`: Cast a vote on an active proposal
- `get-user-vote`: Check a user's vote on a specific proposal
- `proposal-passed`: Determine if a proposal has passed

### Error Codes

- `ERR-NOT-AUTHORIZED (u100)`: User not authorized for the operation
- `ERR-PROPOSAL-EXISTS (u101)`: Proposal ID already exists
- `ERR-PROPOSAL-NOT-FOUND (u102)`: Proposal ID not found
- `ERR-VOTING-CLOSED (u103)`: Voting period has ended
- `ERR-INSUFFICIENT-TOKENS (u104)`: User has insufficient tokens
- `ERR-ALREADY-VOTED (u105)`: User has already voted on this proposal
- `ERR-PROPOSAL-ACTIVE (u106)`: Proposal is still active

## Development

DecentraVote is built using:

- **Clarity**: Smart contract language for the Stacks blockchain
- **Clarinet**: Development environment for Clarity smart contracts
- **Vitest**: Testing framework configured for Clarity development

### Testing

The project uses Vitest with Clarinet integration for comprehensive testing:

```bash
npm run test
```

This runs the test suite defined in the project, validating all contract functionality.

## Security Considerations

- Token-weighted voting ensures that governance power is aligned with economic stake
- Protection against double-voting prevents manipulation
- Time-locked proposals provide adequate time for community deliberation
- All operations are transparent and verifiable on-chain

## Future Enhancements

- Multi-option voting beyond simple yes/no
- Delegation of voting power
- Quadratic voting implementation
- Integration with other DeFi protocols
- Mobile-friendly dApp interface



## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request



---

*DecentraVote - Secure, transparent governance for the decentralized future.*
