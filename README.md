# 🏛️ Lawhub - Decentralized Policy Proposal Platform

A blockchain-based democratic platform where citizens can submit policy proposals and vote on them in a transparent, decentralized manner.

## 🌟 Features

- **👥 Citizen Registration**: Register as a verified citizen to participate
- **📝 Policy Proposals**: Submit detailed policy proposals with stake requirement
- **🗳️ Democratic Voting**: Vote on proposals with weighted voting system
- **⚖️ Reputation System**: Build reputation through participation
- **🎯 Proposal Execution**: Automatic execution of passed proposals
- **🤝 Vote Delegation**: Delegate voting power to trusted representatives

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- STX tokens for staking and transactions

### Installation

```bash
git clone <repository-url>
cd lawhub
clarinet check
```

## 📖 Usage

### 1. Register as Citizen 🆔
```clarity
(contract-call? .Lawhub register-citizen)
```

### 2. Submit a Proposal 📋
```clarity
(contract-call? .Lawhub submit-proposal 
  "Universal Basic Income" 
  "Implement UBI of 1000 STX monthly for all registered citizens")
```

### 3. Vote on Proposals 🗳️
```clarity
;; Vote YES on proposal #1
(contract-call? .Lawhub vote-on-proposal u1 true)

;; Vote NO on proposal #1  
(contract-call? .Lawhub vote-on-proposal u1 false)
```

### 4. Execute Passed Proposals ✅
```clarity
(contract-call? .Lawhub execute-proposal u1)
```

## 🔍 Read-Only Functions

### Get Proposal Details
```clarity
(contract-call? .Lawhub get-proposal u1)
```

### Check Citizen Information
```clarity
(contract-call? .Lawhub get-citizen-info 'SP1234...)
```

### View Proposal Results
```clarity
(contract-call? .Lawhub get-proposal-result u1)
```

### Check Vote Weight
```clarity
(contract-call? .Lawhub get-citizen-vote-weight 'SP1234...)
```

## ⚙️ Contract Parameters

- **Proposal Stake**: 1,000,000 µSTX (1 STX) required to submit proposals
- **Voting Period**: 1,440 blocks (~10 days)
- **Minimum Votes**: 10 votes required for proposal execution
- **Base Vote Weight**: 1 vote per citizen + reputation bonus

## 🏗️ Contract Architecture

### Data Structures

- **Proposals**: Store proposal details, voting results, and execution status
- **Votes**: Track individual votes with weights
- **Citizens**: Maintain citizen registry with reputation scores
- **Stakes**: Handle delegated voting power

### Voting System

The contract implements a weighted voting system where:
- Each citizen gets 1 base vote
- Additional weight based on reputation score
- Delegated votes increase voting power
- Reputation increases through participation

## 🛡️ Security Features

- Stake requirement prevents spam proposals
- One vote per citizen per proposal
- Time-locked voting periods
- Reputation-based vote weighting
- Automatic stake return for successful proposals

## 🎯 Proposal Lifecycle

1. **Submission** 📤: Citizen submits proposal with stake
2. **Voting** 🗳️: Citizens vote during voting period
3. **Execution** ⚡: Proposals with majority support get executed
4. **Settlement** 💰: Stakes returned to successful proposers

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Test with Clarinet
4. Submit pull request

## 📄 License

MIT License - Build the future of democracy! 🌍

